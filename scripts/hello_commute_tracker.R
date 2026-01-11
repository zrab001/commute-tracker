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
  library(yaml)
})

############################################
# Load configuration
############################################

CONFIG <- yaml::read_yaml("config/commute_config.yml")

BUSINESS_TZ <- CONFIG$timezone$business_timezone
HOME_ADDRESS <- CONFIG$address$home_address
WORK_ADDRESS <- CONFIG$address$work_address
TO_WORK_WINDOW <- CONFIG$commute_windows$to_work
TO_HOME_WINDOW <- CONFIG$commute_windows$to_home
HOLIDAY_POLICY <- CONFIG$holiday_policy

############################################
# Validation
############################################

if (!BUSINESS_TZ %in% OlsonNames()) {
  stop("Invalid timezone in config: ", BUSINESS_TZ)
}

############################################
# Holiday classification
############################################

source("R/holiday_classification.R")

############################################
# Google Sheets auth (service account)
############################################

gs4_auth()

############################################
# Helper: HH:MM → seconds
############################################

parse_hhmm_to_seconds <- function(hhmm) {
  parts <- strsplit(as.character(hhmm), ":")[[1]]
  as.integer(parts[1]) * 3600 + as.integer(parts[2]) * 60
}

############################################
# Helper: commute direction
############################################

determine_commute_direction <- function(run_timestamp_local_chr) {

  ts <- as.POSIXct(run_timestamp_local_chr, tz = BUSINESS_TZ)

  sec <- hour(ts) * 3600 + minute(ts) * 60

  if (sec >= parse_hhmm_to_seconds(TO_WORK_WINDOW$start) &&
      sec <= parse_hhmm_to_seconds(TO_WORK_WINDOW$end)) {
    return("to_work")
  }

  if (sec >= parse_hhmm_to_seconds(TO_HOME_WINDOW$start) &&
      sec <= parse_hhmm_to_seconds(TO_HOME_WINDOW$end)) {
    return("to_home")
  }

  NA_character_
}

############################################
# Fallback logger (CSV + console)
############################################

log_route_fallback <- function(
  run_timestamp_local,
  direction,
  origin,
  destination,
  attempted_mode,
  failure_reason,
  outcome
) {

  message(
    "[ROUTE_FALLBACK] ",
    attempted_mode, " | ",
    failure_reason, " | ",
    outcome
  )

  log_entry <- data.frame(
    log_timestamp_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    business_timezone = BUSINESS_TZ,
    run_timestamp_local = run_timestamp_local,
    direction = direction,
    origin_address = origin,
    destination_address = destination,
    attempted_mode = attempted_mode,
    failure_reason = failure_reason,
    outcome = outcome,
    stringsAsFactors = FALSE
  )

  log_file <- "logs/route_fallbacks.csv"

  write.table(
    log_entry,
    file = log_file,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(log_file) || file.size(log_file) == 0,
    append = TRUE
  )
}

############################################
# Extract duration safely
############################################

extract_duration_value <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x) && !is.null(x$value)) return(as.numeric(x$value))
  if (is.data.frame(x) && "value" %in% names(x)) return(as.numeric(x$value[1]))
  NULL
}

############################################
# Google Directions API (robust)
############################################

get_route_duration_seconds <- function(
  origin,
  destination,
  run_timestamp_local,
  direction
) {

  api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  if (api_key == "") stop("GOOGLE_MAPS_API_KEY is not set")

  response <- GET(
    "https://maps.googleapis.com/maps/api/directions/json",
    query = list(
      origin = origin,
      destination = destination,
      mode = "driving",
      departure_time = "now",
      key = api_key
    )
  )

  stop_for_status(response)

  parsed <- fromJSON(content(response, "text", encoding = "UTF-8"))

  message("Directions API status: ", parsed$status)

  if (parsed$status != "OK" || length(parsed$routes) == 0) {
    log_route_fallback(
      run_timestamp_local,
      direction,
      origin,
      destination,
      "traffic",
      "no_routes",
      "abort"
    )
    quit(status = 0)
  }

  legs <- parsed$routes[[1]]$legs

  if (is.null(legs) || length(legs) == 0) {
    log_route_fallback(
      run_timestamp_local,
      direction,
      origin,
      destination,
      "traffic",
      "no_legs",
      "abort"
    )
    quit(status = 0)
  }

  leg <- if (is.data.frame(legs)) legs[1, ] else legs[[1]]

  duration <- extract_duration_value(leg$duration_in_traffic)

  if (!is.null(duration)) {
    return(duration)
  }

  log_route_fallback(
    run_timestamp_local,
    direction,
    origin,
    destination,
    "traffic",
    "no_duration_in_traffic",
    "fallback_non_traffic"
  )

  duration <- extract_duration_value(leg$duration)

  if (!is.null(duration)) {
    log_route_fallback(
      run_timestamp_local,
      direction,
      origin,
      destination,
      "non_traffic",
      "traffic_missing",
      "recovered"
    )
    return(duration)
  }

  log_route_fallback(
    run_timestamp_local,
    direction,
    origin,
    destination,
    "non_traffic",
    "no_duration",
    "abort"
  )

  quit(status = 0)
}

############################################
# Collect base metadata
############################################

collect_commute_metadata <- function() {

  now_posix <- with_tz(Sys.time(), BUSINESS_TZ)

  run_timestamp_local <- format(
    now_posix,
    "%Y-%m-%d %H:%M:%S"
  )

  direction <- determine_commute_direction(run_timestamp_local)

  if (is.na(direction)) {
    message(
      "Run time outside commute windows (",
      format(now_posix, "%H:%M"),
      "). Exiting."
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
# MAIN
############################################

cat("Hello from commute-tracker\n")
cat("Timestamp:", format(Sys.time()), "\n")
cat("Working directory:", getwd(), "\n\n")

commute_df <- collect_commute_metadata()

origin_address <-
  if (commute_df$direction == "to_work") HOME_ADDRESS else WORK_ADDRESS

destination_address <-
  if (commute_df$direction == "to_work") WORK_ADDRESS else HOME_ADDRESS

duration_seconds <- get_route_duration_seconds(
  origin_address,
  destination_address,
  commute_df$run_timestamp_local,
  commute_df$direction
)

commute_df_final <- commute_df %>%
  mutate(
    estimated_duration_seconds = duration_seconds,
    baseline_duration_seconds = duration_seconds,
    preferred_route_current_duration_seconds = duration_seconds,
    override_flag = FALSE,
    preferred_delay_seconds = 0,
    delta_seconds = 0,
    selected_route_id = preferred_route_id,
    origin_address = origin_address,
    destination_address = destination_address,
    event_id = paste0(
      format(as.POSIXct(run_timestamp_local, tz = BUSINESS_TZ),
             "%Y%m%d%H%M%S"),
      "_",
      direction
    ),
    day_type = determine_us_date_classification(
      as.Date(run_timestamp_local),
      HOLIDAY_POLICY$include_black_friday,
      HOLIDAY_POLICY$include_christmas_eve,
      HOLIDAY_POLICY$include_day_after_christmas
    )
  )

############################################
# Append to Google Sheets (idempotent)
############################################

SHEET_ID <- "1H2v-4LtmCDUu534Qo81d8IxZ4efsGJoNZ2b7RaBK2Ro"

existing <- read_sheet(SHEET_ID, range = "A:A", col_names = "event_id")

if (!(commute_df_final$event_id %in% existing$event_id)) {
  sheet_append(SHEET_ID, commute_df_final)
  message("New event appended: ", commute_df_final$event_id)
} else {
  message("Event already exists, skipping append.")
}
