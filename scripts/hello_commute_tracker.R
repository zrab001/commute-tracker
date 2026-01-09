# hello_commute_tracker.R
# Purpose: minimal non-interactive R script for GitHub Actions learning

collect_commute_metadata <- function() {
  data.frame(
    run_timestamp = as.POSIXct(Sys.time(), tz = "America/New_York"),
    runner = Sys.info()[["nodename"]],
    working_directory = getwd(),
    stringsAsFactors = FALSE
  )
}


cat("Hello from commute-tracker\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Working directory:", getwd(), "\n")

result_df <- collect_commute_metadata()
print(result_df)

