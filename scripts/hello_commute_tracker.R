############################################
# Commute Tracker – Hello World Pipeline
# Purpose:
# - Collect canonical commute metadata (seconds)
# - Create a derived, human-readable view (minutes)
# - Prepare for future Google Sheets integration
############################################


############################################
# 1. Function: collect_commute_metadata()
#    - Canonical data
#    - Seconds are the source of truth
############################################

collect_commute_metadata <- function() {

  run_timestamp_local <- Sys.time()
  run_timezone <- "America/New_York"

  commute_df <- data.frame(
    run_timestamp_local = run_timestamp_local,
    run_timezone = run_timezone,
    direction = "to_work",
    route_id = "R1",
    preferred_route_id = "R1",

    # Canonical duration units: SECONDS
    estimated_duration_seconds = 1500,                 # 25 min
    baseline_duration_seconds = 1200,                  # 20 min
    preferred_route_current_duration_seconds = 1800,   # 30 min

    stringsAsFactors = FALSE
  )

  return(commute_df)
}


############################################
# 2. Function: write_to_google_sheets()
#    - Placeholder only
############################################

write_to_google_sheets <- function(df) {
  message("write_to_google_sheets(): not implemented yet")
}

############################################
# 3. Function: decide_route_override()
#    - This logic determines whether to deviate from the preferred route
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
#    - This is where things RUN
############################################

cat("Hello from commute-tracker\n")
cat("Timestamp:", format(Sys.time()), "\n")
cat("Working directory:", getwd(), "\n\n")

# Collect canonical data (seconds)
commute_df <- collect_commute_metadata()

# Derived, human-readable view (minutes)
commute_df_minutes <- transform(
  commute_df,
  estimated_duration_minutes = estimated_duration_seconds / 60,
  baseline_duration_minutes = baseline_duration_seconds / 60,
  preferred_route_current_duration_minutes =
    preferred_route_current_duration_seconds / 60
)

############################################
# 4A. Placeholder route table (multi-route)
#     - Hardcoded for now
#     - Will later come from routing APIs
############################################

routes_df <- data.frame(
  route_id = c("R1", "R2", "R3"),
  estimated_duration_seconds = c(
    commute_df$preferred_route_current_duration_seconds, # preferred
    1500,  # alternative 1
    1650   # alternative 2
  ),
  is_preferred = c(TRUE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

print(routes_df)
cat("\n")

############################################
# 4B. Select best alternative route
############################################

best_alternative_route <- routes_df[
  !routes_df$is_preferred &
    routes_df$estimated_duration_seconds ==
      min(routes_df$estimated_duration_seconds[!routes_df$is_preferred]),
  ]

print(best_alternative_route)
cat("\n")

#Calculating metrics
threshold_seconds <- 120  # 2 minutes

commute_df_metrics <- within(commute_df, {

  # How much worse is the preferred route vs baseline
  preferred_delay_seconds <-
    preferred_route_current_duration_seconds -
    baseline_duration_seconds

  # How much better the best alternative is vs preferred
  delta_seconds <-
    preferred_route_current_duration_seconds -
    estimated_duration_seconds

  delta_pct <-
    delta_seconds / preferred_route_current_duration_seconds

  # Override logic
  override_flag <-
    preferred_delay_seconds >= threshold_seconds &
    estimated_duration_seconds < preferred_route_current_duration_seconds
})


# Print all views
print(commute_df)
cat("\n")
print(commute_df_minutes)
cat("\n")
print(commute_df_metrics)
cat("\n")

# Placeholder side-effect
write_to_google_sheets(commute_df)
