############################################
# Commute Tracker – Production Pipeline
# Config-driven via YAML
############################################

suppressPackageStartupMessages({
  library(googlesheets4)
  library(httr)
  library(jsonlite)
  library(lubridate)
  library(dplyr)
  library(purrr)
  library(magrittr)
  library(yaml)
})

############################################
# Load configuration
############################################

CONFIG <- yaml::read_yaml("config/commute_config.yml")

BUSINESS_TZ <- CONFIG$business_timezone

############################################
# Source holiday classification logic
############################################

source("R/holiday_classification.R")

############################################
# Google auth (Sheets uses service account)
############################################

gs4_auth()

############################################
# Canonical commute addresses
############################################

HOME_ADDRESS <- "7501 Cavan Ct, Laurel, MD 20707"
WORK_ADDRESS <- "8320 Guilford Rd, Columbia, MD 21046"

############################################
# Helper: parse HH:MM strings into seconds
############################################

parse_hhmm_to_seconds <- function(hhmm) {
  parts <- strsplit(hhmm, ":")[[1]]
  as.integer(parts[1]) * 3600 + as.integer(parts[2]) * 60
}

############################################
# Helper: determine commute direction
############################################

determine_commute_direction <- function(run_timestamp_local_chr) {

  ts <- as.POSIXct(run_timestamp_local_chr, tz = BUSINESS_TZ)
  seconds_since_midnight <-
    hour(ts) * 3600 + minute(ts) * 60

  to_work_start <- parse_hhmm_to_seconds(CONFIG$to_work_window$start)
  to_work_end   <- parse_hhmm_to_seconds(CONFIG$to_work_window$end)

  to_home_start <- parse_hhmm_to_seconds(CONFIG$to_home_window$start)
  to_home_end   <- parse_hhmm_to_seconds(CONFIG$to_home_window$end)

  if (seconds_since_midnight >= to_work_start &&
      seconds_since_midnight <= to_work_end) {
    return("to_work")
  }

  if (seconds_since_midnight >= to_home_start &&
      seconds_since_midnight <= to_home_end) {
    return("to_home")
  }

  return(NA_character_)
}

############################################
# Helper: extract duration seconds safely
############################################

extract_duration_value <- function(x) {
  if (is.null(x)) return(NULL)

  if (is.list(x) && !is.null(x$value)) {
    return(as.numeric(x$value))
  }

  if (is.data.frame(x) && "value" %in% names(x)) {
    return(as.numeric(x$value[1]))
  }

  return(NULL)
}

############################################
# Google Directions API call
############################################

get_route_duration_seconds <- function(origin, destination) {

  api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  if (api_key == "") stop("GOOGLE_MAPS_API_KEY is not set")

  response <- httr::GET(
    url = "https://maps.googleapis.com/maps/api/directions/json",
    query = list(
      origin = origin,
      destination = destination,
      mode = "driving",
      departure_time = "now",
      key = api_key
    )
  )

  httr::stop_for_status(response)

  parsed <- jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8")
  )

  if (parsed$status != "OK" ||
      length(parsed$routes) == 0 ||
      length(parsed$routes[[1]]$legs) == 0) {
    stop("Directions API returned no usable route/leg")
  }

  leg <- parsed$routes[[1]]$legs[[1]]

  duration_seconds <- extract_duration_value(leg$duration_in_traffic)
  if (is.null(duration_seconds)) {
    message("Using non-traffic duration fallback")
    duration_seconds <- extract_duration_value(leg$duration)
  }

  if (is.null(duration_seconds) || is.na(duration_seconds)) {
    stop("Directions API returned no usable duration value")
  }

  duration_seconds
}

############################################
# Collect canonical commute metadata
############################################

collect_commute_metadata <- function() {

  run_epoch <- as.numeric(Sys.time())

  run_posix <- as.POSIXct(
    run_epoch,
    origin = "1970-01-01",
    tz = BUSINESS_TZ
  )

  run_timestamp_local <- format(
    run_posix,
    "%Y-%m-%d %H:%M:%S",
    tz = BUSINESS_TZ
  )

  direction <- determine_commute_direction(run_timestamp_local)

  if (is.na(direction)) {
    message(
      "Run time outside commute windows (",
      format(run_posix, "%H:%M"),
      "). No data recorded."
    )
    quit(status = 0)
  }

  data.frame(
    run_timestamp_local = run_timestamp_local,
    run_timezone = BUSINESS_TZ,
    direction = direction,
    route_id = "R1",
    preferred_route_id = "R1",
    stringsAsFactors = FALSE
  )
}

############################################
# Route override logic
############################################

decide_route_override <- function(
  baseline_seconds,
  preferred_seconds,
  best_alternative_seconds
) {
  preferred_delay_seconds <- preferred_seconds - baseline_seconds

  override_flag <-
    preferred_delay_seconds >= CONFIG$route_override_threshold_seconds &
    best_alternative_seconds < preferred_seconds

  list(
    override_flag = override_flag,
    preferred_delay_seconds = preferred_delay_seconds,
    delta_seconds = preferred_seconds - best_alternative_seconds
  )
}

############################################
# MAIN EXECUTION
############################################

cat("Hello from commute-tracker\n")
cat("Timestamp:", format(Sys.time()), "\n")
cat("Working directory:", getwd(), "\n\n")

commute_df <- collect_commute_metadata()

origin_address <-
  if (commute_df$direction == "to_work") HOME_ADDRESS else WORK_ADDRESS

destination_address <-
  if (commute_df$direction == "to_work") WORK_ADDRESS else HOME_ADDRESS

estimated_duration_seconds <-
  get_route_duration_seconds(origin_address, destination_address)

baseline_duration_seconds <- estimated_duration_seconds
preferred_route_current_duration_seconds <- estimated_duration_seconds

decision <- decide_route_override(
  baseline_seconds = baseline_duration_seconds,
  preferred_seconds = preferred_route_current_duration_seconds,
  best_alternative_seconds = estimated_duration_seconds
)

commute_df_decision <- commute_df %>%
  bind_cols(as.data.frame(decision)) %>%
  mutate(
    selected_route_id = ifelse(override_flag, "ALT", preferred_route_id),
    origin_address = origin_address,
    destination_address = destination_address,
    event_id = paste0(
      format(as.POSIXct(run_timestamp_local, tz = BUSINESS_TZ),
             "%Y%m%d%H%M%S"),
      "_",
      direction
    ),
    day_type = determine_us_date_classification(
      date_input_scalar = as.Date(run_timestamp_local),
      include_black_friday = CONFIG$include_black_friday,
      include_christmas_eve = CONFIG$include_christmas_eve,
      include_day_after_christmas = CONFIG$include_day_after_christmas
    )
  )

############################################
# Append to Google Sheets (idempotent)
############################################

SHEET_ID <- "1H2v-4LtmCDUu534Qo81d8IxZ4efsGJoNZ2b7RaBK2Ro"

existing_events <- read_sheet(
  ss = SHEET_ID,
  range = "A:A",
  col_names = "event_id"
)

if (!(commute_df_decision$event_id %in% existing_events$event_id)) {

  sheet_append(
    ss = SHEET_ID,
    data = commute_df_decision
  )

  message("New event appended: ", commute_df_decision$event_id)

} else {

  message("Event already exists, skipping append: ",
          commute_df_decision$event_id)
}
