############################################
# Commute Tracker – Hello World Pipeline
############################################
suppressPackageStartupMessages({
  library(googlesheets4)
})

gs4_auth()

############################################
# 1. Function: collect_commute_metadata()
############################################

collect_commute_metadata <- function() {

  run_timestamp_local <- Sys.time()
  run_timezone <- "America/New_York"

  data.frame(
    run_timestamp_local = run_timestamp_local,
    run_timezone = run_timezone,
    direction = "to_work",
    route_id = "R1",
    preferred_route_id = "R1",

    estimated_duration_seconds = 1500,
    baseline_duration_seconds = 1200,
    preferred_route_current_duration_seconds = 1800,

    stringsAsFactors = FALSE
  )
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
# 3. Main execution block
############################################

cat("Hello from commute-tracker\n")
cat("Timestamp:", format(Sys.time()), "\n")
cat("Working directory:", getwd(), "\n\n")

commute_df <- collect_commute_metadata()

############################################
# 3A. Placeholder multi-route table
############################################

routes_df <- data.frame(
  route_id = c("R1", "R2", "R3"),
  estimated_duration_seconds = c(
    commute_df$preferred_route_current_duration_seconds,
    1500,
    1650
  ),
  is_preferred = c(TRUE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

############################################
# 3B. Best alternative route
############################################

best_alternative_route <- routes_df[
  !routes_df$is_preferred &
    routes_df$estimated_duration_seconds ==
      min(routes_df$estimated_duration_seconds[!routes_df$is_preferred]),
]

best_alternative_seconds <- best_alternative_route$estimated_duration_seconds

############################################
# 3C. Override decision
############################################

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
# 3D. FINAL ROUTE SELECTION (FIX)
############################################

selected_route_id <- if (decision$override_flag) {
  best_alternative_route$route_id
} else {
  commute_df$preferred_route_id
}

############################################
# 3E. Final decision record
############################################

decision_df <- as.data.frame(decision, stringsAsFactors = FALSE)

commute_df_decision <- cbind(
  commute_df,
  decision_df
)

commute_df_decision$selected_route_id <- selected_route_id

commute_df_decision$event_id <- paste0(
  format(commute_df_decision$run_timestamp_local, "%Y%m%d%H%M%S"),
  "_",
  commute_df_decision$direction
)

############################################
# 3F. Derived reporting view (minutes)
############################################

commute_df_minutes <- transform(
  commute_df_decision,
  estimated_duration_minutes = estimated_duration_seconds / 60,
  baseline_duration_minutes = baseline_duration_seconds / 60,
  preferred_route_current_duration_minutes =
    preferred_route_current_duration_seconds / 60
)

############################################
# Output
############################################

print(routes_df)
cat("\n")
print(best_alternative_route)
cat("\n")
print(commute_df_decision)
cat("\n")
print(commute_df_minutes)
cat("\n")

############################################
# Format data frame for append
############################################

commute_df_decision <- commute_df_decision[, c(
  "event_id",
  "run_timestamp_local",
  "run_timezone",
  "direction",
  "route_id",
  "preferred_route_id",
  "estimated_duration_seconds",
  "baseline_duration_seconds",
  "preferred_route_current_duration_seconds",
  "override_flag",
  "preferred_delay_seconds",
  "delta_seconds",
  "selected_route_id"
)]

############################################
# Idempotent append to Google Sheets
############################################

SHEET_ID <- "1H2v-4LtmCDUu534Qo81d8IxZ4efsGJoNZ2b7RaBK2Ro"

# Read existing event_ids (Column A only)
existing_events <- read_sheet(
  ss = SHEET_ID,
  range = "A:A",
  col_names = "event_id"
)

# Check whether this event already exists
event_already_exists <-
  commute_df_decision$event_id %in% existing_events$event_id

# Append only if new
if (!event_already_exists) {

  sheet_append(
    ss = SHEET_ID,
    data = commute_df_decision
  )

  message(
    "New event appended: ",
    commute_df_decision$event_id
  )

} else {

  message(
    "Event already exists, skipping append: ",
    commute_df_decision$event_id
  )

}
