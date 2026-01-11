############################################
# Commute Tracker – Live Traffic Version
############################################

suppressPackageStartupMessages({
  library(googlesheets4)
  library(httr)
  library(jsonlite)
  library(lubridate)
})

# Google Sheets auth (service account via GOOGLE_APPLICATION_CREDENTIALS)
gs4_auth()

# Holiday classification helper
source("R/holiday_classification.R")

############################################
# Canonical addresses
############################################

HOME_ADDRESS <- "7501 Cavan Ct, Laurel, MD 20707"
WORK_ADDRESS <- "8320 Guilford Rd, Columbia, MD 21046"

BUSINESS_TZ <- "America/New_York"

############################################
# 1. Collect base commute metadata
#    IMPORTANT: run_timestamp_local is stored as a CHARACTER string
#    formatted in BUSINESS_TZ to avoid Sheets timezone ambiguity.
############################################

collect_commute_metadata <- function() {

  run_timestamp_epoch <- as.numeric(Sys.time())

  run_timestamp_local_posix <- as.POSIXct(
    run_timestamp_epoch,
    origin = "1970-01-01",
    tz = BUSINESS_TZ
  )

  run_timestamp_local <- format(
    run_timestamp_local_posix,
    "%Y-%m-%d %H:%M:%S",
    tz = BUSINESS_TZ
  )

  data.frame(
    run_timestamp_local = run_timestamp_local,  # <-- CHARACTER in NY time
    run_timezone = BUSINESS_TZ,
    #direction = "to_work",                      # will be automated later
    direction = determine_commute_direction(run_timestamp_local)
    route_id = "R1",
    preferred_route_id = "R1",
    stringsAsFactors = FALSE
  )
}

############################################
# 2. Google Directions API (LIVE, HARDENED)
############################################

get_route_duration_seconds <- function(origin, destination) {

  api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  if (api_key == "") stop("GOOGLE_MAPS_API_KEY is not set", call. = FALSE)

  response <- httr::GET(
    url = "https://maps.googleapis.com/maps/api/directions/json",
    query = list(
      origin = origin,
      destination = destination,
      mode = "driving",
      departure_time = as.integer(Sys.time()),
      traffic_model = "best_guess",
      region = "us",
      key = api_key
    )
  )

  httr::stop_for_status(response)

  parsed <- jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8"),
    simplifyDataFrame = TRUE
  )

  if (parsed$status != "OK" || length(parsed$routes) == 0) {
    stop("Directions API returned no routes. Status = ", parsed$status, call. = FALSE)
  }

  route1 <- parsed$routes[1, ]
  leg <- route1$legs[[1]]

  extract_duration_value <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.data.frame(x) && "value" %in% names(x)) return(x$value[1])
    if (is.list(x) && !is.null(x$value)) return(x$value)
    NULL
  }

  duration_seconds <- extract_duration_value(leg$duration_in_traffic)
  if (is.null(duration_seconds)) duration_seconds <- extract_duration_value(leg$duration)

  if (length(duration_seconds) != 1 || is.na(duration_seconds)) {
    stop("Directions API returned no usable duration value", call. = FALSE)
  }

  as.integer(duration_seconds)
}
############################################
# Helper: determine commute direction
############################################

determine_commute_direction <- function(run_timestamp_local) {

  local_time <- format(run_timestamp_local, "%H:%M")

  if (local_time >= "06:30" && local_time <= "09:30") {
    return("to_work")
  }

  if (local_time >= "15:30" && local_time <= "18:00") {
    return("from_work")
  }

  stop(
    "Run time outside commute windows (",
    local_time,
    "). No data recorded."
  )
}

############################################
# 3. Main execution
############################################

cat("Hello from commute-tracker\n")
cat("Timestamp (NY string):", collect_commute_metadata()$run_timestamp_local, "\n")
cat("Working directory:", getwd(), "\n\n")

commute_df <- collect_commute_metadata()

origin_address <- if (commute_df$direction == "to_work") HOME_ADDRESS else WORK_ADDRESS
destination_address <- if (commute_df$direction == "to_work") WORK_ADDRESS else HOME_ADDRESS

duration_seconds <- get_route_duration_seconds(
  origin = origin_address,
  destination = destination_address
)

############################################
# 4. Final event record
############################################

commute_df_final <- cbind(
  commute_df,
  origin_address = origin_address,
  destination_address = destination_address,
  estimated_duration_seconds = duration_seconds,
  baseline_duration_seconds = duration_seconds,
  preferred_route_current_duration_seconds = duration_seconds,
  override_flag = FALSE,
  preferred_delay_seconds = 0,
  delta_seconds = 0,
  selected_route_id = "R1",
  stringsAsFactors = FALSE
)

# event_id based on the SAME NY-local string
event_stamp <- gsub("[-: ]", "", commute_df_final$run_timestamp_local)  # YYYYMMDDHHMMSS
commute_df_final$event_id <- paste0(event_stamp, "_", commute_df_final$direction)

# Day type derived from NY-local date (string -> Date)
commute_df_final$day_type <- determine_us_date_classification(
  date_input_scalar = as.Date(substr(commute_df_final$run_timestamp_local, 1, 10)),
  include_black_friday = TRUE,
  include_christmas_eve = FALSE,
  include_day_after_christmas = FALSE
)

############################################
# 5. Append to Google Sheets (idempotent)
############################################

SHEET_ID <- "1H2v-4LtmCDUu534Qo81d8IxZ4efsGJoNZ2b7RaBK2Ro"

commute_df_append <- commute_df_final[, c(
  "event_id",
  "run_timestamp_local",
  "run_timezone",
  "direction",
  "origin_address",
  "destination_address",
  "route_id",
  "preferred_route_id",
  "estimated_duration_seconds",
  "baseline_duration_seconds",
  "preferred_route_current_duration_seconds",
  "override_flag",
  "preferred_delay_seconds",
  "delta_seconds",
  "selected_route_id",
  "day_type"
)]

existing_events <- read_sheet(
  ss = SHEET_ID,
  range = "A:A",
  col_names = "event_id"
)

if (!(commute_df_append$event_id %in% existing_events$event_id)) {

  sheet_append(ss = SHEET_ID, data = commute_df_append)
  message("New event appended: ", commute_df_append$event_id)

} else {

  message("Event already exists, skipping append: ", commute_df_append$event_id)
}
