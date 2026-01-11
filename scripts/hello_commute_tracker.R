############################################
# Commute Tracker – Hello World Pipeline
# v1.1: Canonical addresses + route definitions
############################################

suppressPackageStartupMessages({
  library(googlesheets4)
})

suppressPackageStartupMessages({
  library(googlesheets4)
})

source("R/holiday_classification.R")

gs4_auth()

############################################
# Canonical commute addresses
############################################

HOME_ADDRESS <- "7501 Cavan Ct, Laurel, MD 20707"
WORK_ADDRESS <- "8320 Guilford Rd, Columbia, MD 21046"

############################################
# Canonical route definitions (intent only)
############################################

route_definitions <- data.frame(
  route_id = c("R1", "R2", "R3"),
  route_label = c(
    "Default / fastest route",
    "Avoid highways",
    "Local roads"
  ),
  stringsAsFactors = FALSE
)

############################################
# 1. Function: collect_commute_metadata()
############################################

# ---- MOCKED PLACEHOLDER DURATIONS (DISABLED) ----
# The following block is intentionally disabled.
# It remains here as a fallback for offline testing.
#
# collect_commute_metadata <- function() {
#
#   run_timestamp_local <- Sys.time()
#   run_timezone <- "America/New_York"
#
#   data.frame(
#     run_timestamp_local = run_timestamp_local,
#     run_timezone = run_timezone,
#     direction = "to_work",
#     route_id = "R1",
#     preferred_route_id = "R1",
#
#     # Placeholder durations (seconds)
#     estimated_duration_seconds = 1500,
#     baseline_duration_seconds = 1200,
#     preferred_route_current_duration_seconds = 1800,
#
#     stringsAsFactors = FALSE
#   )
# }

collect_commute_metadata <- function(direction = "to_work") {

  run_timestamp_local <- Sys.time()
  run_timezone <- "America/New_York"

  data.frame(
    run_timestamp_local = run_timestamp_local,
    run_timezone = run_timezone,
    direction = direction,

    # These are now placeholders to be populated later
    route_id = "R1",
    preferred_route_id = "R1",

    estimated_duration_seconds = NA_real_,
    baseline_duration_seconds = NA_real_,
    preferred_route_current_duration_seconds = NA_real_,

    stringsAsFactors = FALSE
  )
}


get_route_duration_seconds <- function(origin, destination) {

  api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  if (api_key == "") {
    stop("GOOGLE_MAPS_API_KEY is not set")
  }

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

  if (parsed$status != "OK") {
    stop("Directions API error: ", parsed$status)
  }

  parsed$routes[[1]]$legs[[1]]$duration$value
}





############################################
# 2. Function: decide_route_override()
############################################

decide_route_override <- function(
  baseline_seconds,
  preferred_seconds,
  best_alternative_seconds,
  threshold_seconds = 120
) {
  preferred_delay_seconds <- preferred_seconds - baseline_seconds

  override_flag <-
    preferred_delay_seconds >= threshold_seconds &
    best_alternative_seconds < preferred_seconds

  list(
    override_flag = override_flag,
    preferred_delay_seconds = preferred_delay_seconds,
    delta_seconds = preferred_seconds - best_alternative_seconds
  )
}

############################################
# 3. Function: get_route_durations() (mocked)
############################################

get_route_durations <- function(
  origin_address,
  destination_address,
  route_definitions
) {

  # NOTE:
  # This is a MOCK implementation.
  # It will later be replaced by real routing APIs.

  data.frame(
    route_id = route_definitions$route_id,
    estimated_duration_seconds = c(
      1800,  # R1: preferred / default
      1500,  # R2: alternative
      1650   # R3: alternative
    ),
    is_preferred = route_definitions$route_id == "R1",
    stringsAsFactors = FALSE
  )
}


############################################
# 3. Main execution block
############################################

cat("Hello from commute-tracker\n")
cat("Timestamp:", format(Sys.time()), "\n")
cat("Working directory:", getwd(), "\n\n")

############################################
# 3A. Collect base commute metadata
############################################

commute_df <- collect_commute_metadata()

############################################
# 3B. Get route durations (mocked)
############################################

#routes_df <- get_route_durations(
#  origin_address = HOME_ADDRESS,
#  destination_address = WORK_ADDRESS,
#  route_definitions = route_definitions
#)

routes_df <- data.frame(
  route_id = c("R1"),
  estimated_duration_seconds = c(
    get_route_duration_seconds(
      origin = if (commute_df$direction == "to_work") HOME_ADDRESS else WORK_ADDRESS,
      destination = if (commute_df$direction == "to_work") WORK_ADDRESS else HOME_ADDRESS
    )
  ),
  is_preferred = TRUE,
  stringsAsFactors = FALSE
)


############################################
# 3C. Select best alternative route
############################################

best_alternative_route <- routes_df[
  !routes_df$is_preferred &
    routes_df$estimated_duration_seconds ==
      min(routes_df$estimated_duration_seconds[!routes_df$is_preferred]),
]

best_alternative_seconds <- best_alternative_route$estimated_duration_seconds

############################################
# 3D. Override decision
############################################

if (is.na(commute_df$baseline_duration_seconds)) {
  commute_df$baseline_duration_seconds <- routes_df$estimated_duration_seconds[1]
  commute_df$preferred_route_current_duration_seconds <- routes_df$estimated_duration_seconds[1]
}

decision <- decide_route_override(
  baseline_seconds =
    commute_df$baseline_duration_seconds,
  preferred_seconds =
    commute_df$preferred_route_current_duration_seconds,
  best_alternative_seconds =
    best_alternative_seconds,
  threshold_seconds = 120
)

############################################
# 3E. Final route selection
############################################

selected_route_id <- if (decision$override_flag) {
  best_alternative_route$route_id
} else {
  commute_df$preferred_route_id
}

############################################
# 3F. Final decision record (canonical event)
############################################

decision_df <- as.data.frame(decision, stringsAsFactors = FALSE)

commute_df_decision <- cbind(
  commute_df,
  decision_df
)

commute_df_decision$selected_route_id <- selected_route_id
commute_df_decision$origin_address <- HOME_ADDRESS
commute_df_decision$destination_address <- WORK_ADDRESS

commute_df_decision$event_id <- paste0(
  format(commute_df_decision$run_timestamp_local, "%Y%m%d%H%M%S"),
  "_",
  commute_df_decision$direction
)

commute_df_decision$day_type <- determine_us_date_classification(
  date_input_scalar = as.Date(commute_df_decision$run_timestamp_local),
  include_black_friday = TRUE,
  include_christmas_eve = FALSE,
  include_day_after_christmas = FALSE
)


################################################
#Stop code if determine_us_date_classification() remains undefined or return values are invalid
####################################################
stopifnot(!is.na(commute_df_decision$day_type))

stopifnot(
  commute_df_decision$day_type %in%
    c("HARD", "SOFT", "WORKDAY", "WEEKEND", "HOLIWEEKEND")
)

############################################
# 3G. Derived reporting view (minutes)
############################################

commute_df_minutes <- transform(
  commute_df_decision,
  estimated_duration_minutes = estimated_duration_seconds / 60,
  baseline_duration_minutes = baseline_duration_seconds / 60,
  preferred_route_current_duration_minutes =
    preferred_route_current_duration_seconds / 60
)

############################################
# Diagnostics (console only)
############################################

print(route_definitions)
cat("\n")
print(routes_df)
cat("\n")
print(best_alternative_route)
cat("\n")
print(commute_df_decision)
cat("\n")
print(commute_df_minutes)
cat("\n")

############################################
# Format data frame for append (schema-aligned)
############################################

commute_df_append <- commute_df_decision[, c(
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

############################################
# Idempotent append to Google Sheets
############################################

SHEET_ID <- "1H2v-4LtmCDUu534Qo81d8IxZ4efsGJoNZ2b7RaBK2Ro"

existing_events <- read_sheet(
  ss = SHEET_ID,
  range = "A:A",
  col_names = "event_id"
)

event_already_exists <-
  commute_df_decision$event_id %in% existing_events$event_id

if (!event_already_exists) {

  sheet_append(
  ss = SHEET_ID,
  data = commute_df_append
  )

  message("New event appended: ", commute_df_decision$event_id)

} else {

  message("Event already exists, skipping append: ", commute_df_decision$event_id)

}
