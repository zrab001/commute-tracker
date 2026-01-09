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

    override_flag = TRUE,

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
# 3. Main execution block
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

#Calculating metrics
commute_df_metrics <- transform(
  commute_df,
  delta_seconds = estimated_duration_seconds - baseline_duration_seconds,
  delta_pct = (estimated_duration_seconds - baseline_duration_seconds) /
              baseline_duration_seconds
)

# Print all views
print(commute_df)
cat("\n")
print(commute_df_minutes)
cat("\n")
print(commute_df_metrics)
cat("\n")

# Placeholder side-effect
write_to_google_sheets(commute_df)
