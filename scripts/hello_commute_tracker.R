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
  library(tibble)
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

if (!BUSINESS_TZ %in% OlsonNames()) {
  stop("Invalid business timezone: ", BUSINESS_TZ)
}

############################################
# Source holiday classification
############################################

source("R/holiday_classification.R")

############################################
# Google Sheets auth (non-interactive)
############################################

gs4_auth(path = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS"))

############################################
# Utility: HH:MM → seconds
############################################

parse_hhmm_to_seconds <- function(hhmm) {
  parts <- strsplit(as.character(hhmm), ":")[[1]]
  as.integer(parts[1]) * 3600 + as.integer(parts[2]) * 60
}

############################################
# Determine commute direction
############################################

determine_commute_direction <- function(run_ts_chr) {

  ts <- as.POSIXct(run_ts_chr, tz = BUSINESS_TZ)

  seconds_since_midnight <- hour(ts) * 3600 + minute(ts) * 60

  to_work_start <- parse_hhmm_to_seconds(TO_WORK_WINDOW$start)
  to_work_end   <- parse_hhmm_to_seconds(TO_WORK_WINDOW$end)

  to_home_start <- parse_hhmm_to_seconds(TO_HOME_WINDOW$start)
  to_home_end   <- parse_hhmm_to_seconds(TO_HOME_WINDOW$end)

  if (seconds_since_midnight >= to_work_start &&
      seconds_since_midnight <= to_work_end) {
    return("to_work")
  }

  if (seconds_since_midnight >= to_home_start &&
      seconds_since_midnight <= to_home_end) {
    return("to_home")
  }

  NA_character_
}

############################################
# ROUTE FALLBACK LOGGER (console + CSV)
############################################

log_route_fallback <- function(
  attempt,
  reason,
  action,
  origin,
  destination
) {
  log_path <- "logs/route_fallback_log.csv"
  dir.create("logs", showWarnings = FALSE)

  entry <- tibble(
    timestamp_utc = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    attempt = attempt,
    reason = reason,
    action = action,
    origin = origin,
    destination = destination
  )

  if (!file.exists(log_path)) {
    write.table(
      entry,
      log_path,
      sep = ",",
      row.names = FALSE,
      col.names = TRUE,
      append = FALSE
    )
  } else {
    write.table(
      entry,
      log_path,
      sep = ",",
      row.names = FALSE,
      col.names = FALSE,
      append = TRUE
    )
  }

  message(sprintf(
    "[ROUTE_FALLBACK] %s | %s | %s",
    attempt, reason, action
  ))
}

############################################
# Safe duration extraction
############################################

extract_duration_value <- function(x) {
  if (is.null(x)) return(NULL)

  if (is.list(x) && !is.null(x$value)) {
    return(as.numeric(x$value))
  }

  if (is.data.frame(x) && "value" %in% names(x)) {
    return(as.numeric(x$value[1]))
  }

  NULL
}

############################################
# Google Directions API
############################################

get_route_duration_seconds <- function(origin, destination) {

  api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  if (api_key == "") stop("GOOGLE_MAPS_API_KEY is not set")

  response <- httr::GET(
    "https://maps.googleapis.com/maps/api/directions/json",
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

  message("Directions API status: ", parsed$status)

  if (parsed$status != "OK" || length(parsed$routes) == 0) {
    log_route_fallback(
      "traffic", "no_routes", "abort", origin, destination
    )
    quit(status = 0)
  }

  legs <- parsed$routes[[1]]$legs

  if (is.null(legs) || length(legs) == 0) {
    log_route_fallback(
      "traffic", "no_legs", "abort", origin, destination
    )
    quit(status = 0)
  }

  leg <- if (is.data.frame(legs)) legs[1, ] else legs[[1]]

  duration_seconds <- extract_duration_value(leg$duration_in_traffic)

  if (is.null(duration_seconds)) {
    log_route_fallback(
      "traffic", "no_duration_in_traffic", "fallback", origin, destination
    )
    duration_seconds <- extract_duration_value(leg$duration)
  }

  if (is.null(duration_seconds) || is.na(duration_seconds)) {
    log_route_fallback(
      "fallback", "no_duration", "abort", origin, destination
    )
    quit(status = 0)
  }

  duration_seconds
}

############################################
# Collect commute metadata
############################################

collect_commute_metadata <- function() {

  run_posix <- with_tz(Sys.time(), BUSINESS_TZ)

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

  tibble(
    run_timestamp_local = run_timestamp_local,
    run_timezone = BUSINESS_TZ,
    direction = direction,
    route_id = "R1",
    preferred_route_id = "R1"
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

duration_seconds <-
  get_route_duration_seconds(origin_address, destination_address)

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
      date_input_scalar = as.Date(run_timestamp_local),
      include_black_friday = HOLIDAY_POLICY$include_black_friday,
      include_christmas_eve = HOLIDAY_POLICY$include_christmas_eve,
      include_day_after_christmas =
        HOLIDAY_POLICY$include_day_after_christmas
    )
  )

############################################
# Append to Google Sheet (idempotent)
############################################

SHEET_ID <- "1H2v-4LtmCDUu534Qo81d8IxZ4efsGJoNZ2b7RaBK2Ro"

existing_events <- read_sheet(
  ss = SHEET_ID,
  range = "A:A",
  col_names = "event_id"
)

if (!(commute_df_final$event_id %in% existing_events$event_id)) {

  sheet_append(
    ss = SHEET_ID,
    data = commute_df_final
  )

  message("New event appended: ", commute_df_final$event_id)

} else {

  message("Event already exists, skipping append: ",
          commute_df_final$event_id)
}
