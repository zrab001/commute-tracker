############################################
# Commute Tracker – Hello World Pipeline
# Config-driven version
############################################

suppressPackageStartupMessages({
  library(yaml)
  library(dplyr)
  library(googlesheets4)
  library(httr)
  library(jsonlite)
})

# --- Load configuration -----------------------------------------------------

CONFIG_PATH <- "config/commute_config.yml"
if (!file.exists(CONFIG_PATH)) {
  stop("Config file not found: ", CONFIG_PATH)
}

CONFIG <- yaml::read_yaml(CONFIG_PATH)

# Pull config values (fail fast if missing)
BUSINESS_TZ <- CONFIG$timezone$business_tz
if (is.null(BUSINESS_TZ) || !nzchar(BUSINESS_TZ)) stop("Config missing timezone.business_tz")

WINDOWS <- CONFIG$commute_windows
if (is.null(WINDOWS$to_work) || is.null(WINDOWS$to_home)) stop("Config missing commute_windows.to_work/to_home")

HOLIDAY_POLICY <- CONFIG$holiday_policy
if (is.null(HOLIDAY_POLICY)) stop("Config missing holiday_policy")

INCLUDE_BLACK_FRIDAY <- isTRUE(HOLIDAY_POLICY$include_black_friday)
INCLUDE_CHRISTMAS_EVE <- isTRUE(HOLIDAY_POLICY$include_christmas_eve)
INCLUDE_DAY_AFTER_CHRISTMAS <- isTRUE(HOLIDAY_POLICY$include_day_after_christmas)

# --- Source holiday classification function --------------------------------

source("R/holiday_classification.R")

# --- Google Sheets auth -----------------------------------------------------

gs4_auth()

# --- Canonical commute addresses -------------------------------------------

HOME_ADDRESS <- "7501 Cavan Ct, Laurel, MD 20707"
WORK_ADDRESS <- "8320 Guilford Rd, Columbia, MD 21046"

# --- Canonical route definitions (intent only) -----------------------------

route_definitions <- data.frame(
  route_id = c("R1", "R2", "R3"),
  route_label = c(
    "Default / fastest route",
    "Avoid highways",
    "Local roads"
  ),
  stringsAsFactors = FALSE
)

# ----------------------------------------------------------------------------
# Helpers (time parsing + direction guardrails)
# ----------------------------------------------------------------------------

parse_hhmm_to_minutes <- function(time_hhmm) {
  # Developer-friendly: accepts "9:30" or "09:30"
  if (length(time_hhmm) != 1 || is.null(time_hhmm) || !nzchar(time_hhmm)) {
    stop("Time window value must be a single non-empty string like '06:30' or '9:30'")
  }

  m <- regexec("^\\s*(\\d{1,2})\\s*:\\s*(\\d{2})\\s*$", time_hhmm)
  parts <- regmatches(time_hhmm, m)[[1]]

  if (length(parts) != 3) stop("Invalid time format: '", time_hhmm, "'. Use HH:MM (24-hour).")

  hh <- as.integer(parts[2])
  mm <- as.integer(parts[3])

  if (is.na(hh) || is.na(mm) || hh < 0 || hh > 23 || mm < 0 || mm > 59) {
    stop("Invalid time value: '", time_hhmm, "'.")
  }

  hh * 60 + mm
}

time_to_minutes_local <- function(run_timestamp_local_chr) {
  # run_timestamp_local_chr expected "YYYY-MM-DD HH:MM:SS"
  hhmm <- substr(run_timestamp_local_chr, 12, 16)
  parse_hhmm_to_minutes(hhmm)
}

determine_commute_direction <- function(run_timestamp_local_chr) {
  # Returns "to_work", "to_home", or NA if outside windows
  minute_of_day <- time_to_minutes_local(run_timestamp_local_chr)

  to_work_start <- parse_hhmm_to_minutes(WINDOWS$to_work$start)
  to_work_end   <- parse_hhmm_to_minutes(WINDOWS$to_work$end)

  to_home_start <- parse_hhmm_to_minutes(WINDOWS$to_home$start)
  to_home_end   <- parse_hhmm_to_minutes(WINDOWS$to_home$end)

  if (minute_of_day >= to_work_start && minute_of_day <= to_work_end) return("to_work")
  if (minute_of_day >= to_home_start && minute_of_day <= to_home_end) return("to_home")

  NA_character_
}

# ----------------------------------------------------------------------------
# Directions API: get traffic-aware duration (seconds)
# ----------------------------------------------------------------------------

extract_duration_value <- function(duration_obj) {
  # duration_obj may be list with $value, or NULL
  if (is.null(duration_obj)) return(NULL)
  if (is.list(duration_obj) && !is.null(duration_obj$value) && is.numeric(duration_obj$value)) {
    return(as.integer(duration_obj$value))
  }
  NULL
}

get_route_duration_seconds <- function(origin, destination) {

  api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  if (!nzchar(api_key)) stop("GOOGLE_MAPS_API_KEY is not set (GitHub secret + workflow env)")

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

  if (!identical(parsed$status, "OK")) {
    stop("Directions API error: ", parsed$status)
  }

  # Prefer duration_in_traffic if available, fall back to duration
  leg <- parsed$routes[[1]]$legs[[1]]

  duration_seconds <- extract_duration_value(leg$duration_in_traffic)
  if (is.null(duration_seconds)) {
    duration_seconds <- extract_duration_value(leg$duration)
  }

  if (is.null(duration_seconds) || !is.finite(duration_seconds) || duration_seconds <= 0) {
    stop("Directions API returned no usable duration value")
  }

  duration_seconds
}

# ----------------------------------------------------------------------------
# 1. Collect base commute metadata
#    IMPORTANT: run_timestamp_local is stored as CHARACTER in BUSINESS_TZ
# ----------------------------------------------------------------------------

collect_commute_metadata <- function() {

  # Make a NY-local timestamp string deterministically (avoid UTC surprises)
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

  direction <- determine_commute_direction(run_timestamp_local)

  if (is.na(direction)) {
    message(
      "Exiting without writing data (outside commute window): ",
      substr(run_timestamp_local, 12, 16),
      " ",
      BUSINESS_TZ
    )
    quit(status = 0)
  }

  # Canonical record (durations filled later)
  data.frame(
    run_timestamp_local = run_timestamp_local,
    run_timezone = BUSINESS_TZ,
    direction = direction,
    route_id = "R1",
    preferred_route_id = "R1",
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------------------
# 2. Decide route override
# ----------------------------------------------------------------------------

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

# ----------------------------------------------------------------------------
# 3. get_route_durations() (MOCK fallback)
# ----------------------------------------------------------------------------

get_route_durations_mock <- function(route_definitions) {
  data.frame(
    route_id = route_definitions$route_id,
    estimated_duration_seconds = c(
      1800,  # R1 preferred
      1500,  # R2
      1650   # R3
    ),
    is_preferred = route_definitions$route_id == "R1",
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------------

cat("Hello from commute-tracker\n")
cat("Timestamp:", format(Sys.time()), "\n")
cat("Working directory:", getwd(), "\n\n")

commute_df <- collect_commute_metadata()

# Determine origin/destination based on direction
origin_address <- if (commute_df$direction == "to_work") HOME_ADDRESS else WORK_ADDRESS
destination_address <- if (commute_df$direction == "to_work") WORK_ADDRESS else HOME_ADDRESS

# Live duration (R1 only for now)
duration_seconds <- get_route_duration_seconds(
  origin = origin_address,
  destination = destination_address
)

routes_df <- data.frame(
  route_id = "R1",
  estimated_duration_seconds = duration_seconds,
  is_preferred = TRUE,
  stringsAsFactors = FALSE
)

# Until we have multiple live alternatives, keep baseline/preferred values aligned
commute_df$estimated_duration_seconds <- duration_seconds
commute_df$baseline_duration_seconds <- duration_seconds
commute_df$preferred_route_current_duration_seconds <- duration_seconds

# Best alternative placeholder (none yet)
best_alternative_seconds <- duration_seconds

decision <- decide_route_override(
  baseline_seconds = commute_df$baseline_duration_seconds,
  preferred_seconds = commute_df$preferred_route_current_duration_seconds,
  best_alternative_seconds = best_alternative_seconds,
  threshold_seconds = 120
)

selected_route_id <- if (decision$override_flag) {
  "R2" # placeholder until we have real alternatives
} else {
  commute_df$preferred_route_id
}

decision_df <- as.data.frame(decision, stringsAsFactors = FALSE)

commute_df_decision <- cbind(commute_df, decision_df)

commute_df_decision$selected_route_id <- selected_route_id
commute_df_decision$origin_address <- origin_address
commute_df_decision$destination_address <- destination_address

# event_id based on NY-local timestamp string
commute_df_decision$event_id <- paste0(
  gsub("[-: ]", "", commute_df_decision$run_timestamp_local),
  "_",
  commute_df_decision$direction
)

# day_type from config-driven holiday policy
commute_df_decision$day_type <- determine_us_date_classification(
  date_input_scalar = substr(commute_df_decision$run_timestamp_local, 1, 10),
  include_black_friday = INCLUDE_BLACK_FRIDAY,
  include_christmas_eve = INCLUDE_CHRISTMAS_EVE,
  include_day_after_christmas = INCLUDE_DAY_AFTER_CHRISTMAS
)

stopifnot(!is.na(commute_df_decision$day_type))
stopifnot(commute_df_decision$day_type %in% c("HARD", "SOFT", "WORKDAY", "WEEKEND", "HOLIWEEKEND"))

# Schema-aligned append df (must match sheet headers)
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

# Idempotent append
SHEET_ID <- "1H2v-4LtmCDUu534Qo81d8IxZ4efsGJoNZ2b7RaBK2Ro"

existing_events <- read_sheet(
  ss = SHEET_ID,
  range = "A:A",
  col_names = "event_id"
)

event_already_exists <- commute_df_append$event_id %in% existing_events$event_id

if (!event_already_exists) {
  sheet_append(ss = SHEET_ID, data = commute_df_append)
  message("New event appended: ", commute_df_append$event_id)
} else {
  message("Event already exists, skipping append: ", commute_df_append$event_id)
}

# MySQL-ready uniqueness constraint (one-liner, for your notes / DDL)
message("MySQL constraint: UNIQUE KEY uq_commute_event_id (event_id)")
