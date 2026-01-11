############################################
# Commute Tracker – Live Traffic Version
############################################

suppressPackageStartupMessages({
  library(googlesheets4)
  library(httr)
  library(jsonlite)
  library(lubridate)
})

# Authenticate to Google Sheets via service account
gs4_auth()

# Holiday classification helper
source("R/holiday_classification.R")

############################################
# Canonical addresses
############################################

HOME_ADDRESS <- "7501 Cavan Ct, Laurel, MD 20707"
WORK_ADDRESS <- "8320 Guilford Rd, Columbia, MD 21046"

############################################
# 1. Collect base commute metadata
############################################

collect_commute_metadata <- function() {

  run_timestamp_local <- Sys.time()
  run_timezone <- "America/New_York"

  data.frame(
    run_timestamp_local = run_timestamp_local,
    run_timezone = run_timezone,
    direction = "to_work",     # will be automated later
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
  if (api_key == "") {
    stop("GOOGLE_MAPS_API_KEY is not set", call. = FALSE)
  }

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
    stop(
      "Directions API returned no routes. Status = ",
      parsed$status,
      call. = FALSE
    )
  }

  # ---- SAFE LEG EXTRACTION (THIS WAS THE MISSING PIECE) ----
  route1 <- parsed$routes[1, ]
  leg <- route1$legs[[1]]

  extract_duration_value <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.data.frame(x) && "value" %in% names(x)) return(x$value[1])
    if (is.list(x) && !is.null(x$value)) return(x$value)
    NULL
  }

  duration_seconds <- extract_duration_value(leg$duration_in_traffic)

  if (is.null(duration_seconds)) {
    duration_seconds <- extract_duration_value(leg$duration)
  }

  if (length(duration_seconds) != 1 || is.na(duration_seconds)) {
    stop("Directions API returned no usable duration value", call. = FALSE)
  }

  as.integer(duration_seconds)
}

############################################
# 3. Main execution
############################################

cat("Hello from commute-tracker\n")
cat("Timestamp:", format(Sys.time()), "\n")
cat("Working directory:", getwd(), "\n\n")

# Base metadata
commute_df <- collect_commute_metadata()

# Resolve direction
origin_address <- if (commute_df$direction == "to_work") HOME_ADDRESS else WORK_ADDRESS
destination_address <- if (commute_df$direction == "to_work") WORK_ADDRESS else HOME_ADDRESS

# Live traffic duration
duration_seconds <- get_route_duration_seconds(
  origin = origin_address,
  destination = destination_address
)

############################################
# 4. Canonical event record
############################################

commute_df_final <- cbind(
  commute_df,
  estimated_duration_seconds = duration_seconds,
  baseline_duration_seconds = duration_seconds,
  preferred_route_current_duration_seconds = duration_seconds,
  override_flag = FALSE,
  preferred_delay_seconds = 0,
  delta_seconds = 0,
  selected_route_id = "R1",
  origin_address = origin_address,
  destination_address = destination_address,
  stringsAsFactors = FALSE
)

# Deterministic event ID
commute_df_final$event_id <- paste0(
  format(commute_df_final$run_timestamp_local, "%Y%m%d%H%M%S"),
  "_",
  commute_df_final$direction
)

# Day classification
commute_df_final$day_type <- determine_us_date_classification(
  date_input_scalar = as.Date(commute_df_final$run_timestamp_local),
  include_black_friday = TRUE,
  include_christmas_eve = FALSE,
  include_day_after_christmas = FALSE
)

############################################
# 5. Diagnostics (console only)
############################################

print(commute_df_final)
cat("\n")

############################################
# 6. Append to Google Sheets (idempotent)
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

  sheet_append(
    ss = SHEET_ID,
    data = commute_df_append
  )

  message("New event appended: ", commute_df_append$event_id)

} else {

  message("Event already exists, skipping append: ", commute_df_append$event_id)
}
