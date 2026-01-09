############################################
# Commute Tracker – Hello World Pipeline
############################################
suppressPackageStartupMessages({
  library(dplyr)
  library(googlesheets4)
})

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
# 2. Function: write_to_google_sheets()
############################################

write_to_google_sheets <- function(df) {
  message("write_to_google_sheets(): not implemented yet")
}

############################################
# 3. Function: decide_route_override()
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
# 4. Main execution block
############################################

cat("Hello from commute-tracker\n")
cat("Timestamp:", format(Sys.time()), "\n")
cat("Working directory:", getwd(), "\n\n")

commute_df <- collect_commute_metadata()

############################################
# 4A. Placeholder multi-route table
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
# 4B. Best alternative route
############################################

best_alternative_route <- routes_df[
  !routes_df$is_preferred &
    routes_df$estimated_duration_seconds ==
      min(routes_df$estimated_duration_seconds[!routes_df$is_preferred]),
]

best_alternative_seconds <- best_alternative_route$estimated_duration_seconds

############################################
# 4C. Override decision
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
# 4D. FINAL ROUTE SELECTION  ⭐ NEW ⭐
############################################

selected_route_id <- if (decision$override_flag) {
  best_alternative_route$route_id
} else {
  commute_df$preferred_route_id
}

############################################
# 4E. Final decision record
############################################

commute_df_final <- cbind(
  commute_df,
  decision,
  selected_route_id = selected_route_id
)

############################################
# 4F. Derived reporting view (minutes)
############################################

commute_df_minutes <- transform(
  commute_df_final,
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
print(commute_df_final)
cat("\n")
print(commute_df_minutes)
cat("\n")

write_to_google_sheets(commute_df_final)
