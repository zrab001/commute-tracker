############################################
# Commute Tracker – Live Directions Version
############################################

suppressPackageStartupMessages({
  library(googlesheets4)
  library(httr)
  library(jsonlite)
  library(lubridate)
})

# --- Auth for Google Sheets (service account) ---
gs4_auth()

# --- Holiday classification helper ---
source("R/holiday_classification.R")

############################################
# Canonical addresses
############################################

HOME_ADDRESS <- "7501 Cavan Ct, Laurel, MD 20707"
WORK_ADDRESS <- "8320 Guilford Rd, Columbia, MD 21046"

############################################
# 1. collect_commute_metadata()
############################################

collect_commute_metadata <- function() {

  run_timestamp_local <- Sys.time()
  run_timezone <- "America/New_York"

  data.frame(
    run_timestamp_local = run_timestamp_local,
    run_timezone = run_timezone,
    direction = "to_work",           # will be automated later
    route_id = "R1",
    preferred_route_id = "R1",
    stringsAsFactors = FALSE
  )
}

############################################
# 2. Google Directions API call (hardened)
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
      departure_time = "now",
      region = "us",
      key = api_key
    )
  )

  httr::stop_for_status(response)

  parsed <- jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8")
  )

  if (parsed$status != "OK" || length(parsed$routes) == 0) {
    stop(
      "Directions API returned no routes. Status = ",
      parsed$status,
      call. = FALSE
    )
  }

duration_value <- parsed$routes[[1]]$legs[[1]]$duration$value

if (length(duration_value) != 1 || is.na(duration_value)) {
  stop(
    "Directions API returned invalid duration value",
    call. = FALSE
  )
}

duration_value
}

############################################
# 3. Main execution
############################################

cat("Hello from commute-tracker\n")
cat("Timestamp:", format(Sys.time()), "\n")
cat("Working directory:", getwd(), "\n\n")

# --- Base metadata ---
commute_df <- collect_commute_metadata()

# --- Resolve direction ---
origin_address <- if (commute_df$direction == "to_work") HOME_ADDRESS else WORK_ADDRESS
destination_address <- if (commute_df$direction == "to_work") WORK_ADDRESS else HOME_ADDRESS

# --- Call Directions API (SAFE) ---
duration_seconds <- get_route_duration_seconds(
  origin = origin_address,
  destination = destination_address
)

# --- Build canonical route table (single live route for now) ---
routes_df <- data.frame(
  route_id = "R1",
  estimated_duration_seconds = duration_seconds,
  is_preferred = TRUE,
  stringsAsFactors = FALSE
)

############################################
# 4. Final event record (no override yet)
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

# --- Event ID ---
commute_df_final$event_id <- paste0(
  format(commute_df_final$run_timestamp_local, "%Y%m%d%H%M%S"),
  "_",
  commute_df_final$direction
)

# --- Day type classification ---
commute_df_final$day_type <- determine_us_date_classification(
  date_input_scalar = as.Date(commute_df_final$run_timestamp_local),
  include_black_friday = TRUE,
  include_christmas_eve = FALSE,
  include_day_after_christmas = FALSE
)

############################################
# 5. Diagnostics
############################################

print(routes_df)
cat("\n")
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
