# hello_commute_tracker.R
# Purpose: minimal non-interactive R script for GitHub Actions learning

write_to_google_sheets <- function(df) {
  message("write_to_google_sheets(): not implemented yet")
}

collect_commute_metadata <- function() {

  data.frame(
    run_timestamp_local = as.POSIXct(Sys.time(), tz = "America/New_York"),
    run_timezone = "America/New_York",

    direction = "to_work",

    route_id = "R1",
    preferred_route_id = "R1",

    estimated_duration_seconds = 1500,
    baseline_duration_seconds  = 1200,
    preferred_route_current_duration_seconds = 1800,
    
    override_flag = TRUE,

    stringsAsFactors = FALSE
  )

}


cat("Hello from commute-tracker\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Working directory:", getwd(), "\n")

result_df <- collect_commute_metadata()
print(result_df)

write_to_google_sheets(result_df)
