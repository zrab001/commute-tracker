# hello_commute_tracker.R
# Purpose: minimal non-interactive R script for GitHub Actions learning

cat("Hello from commute-tracker\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Working directory:", getwd(), "\n")

result_df <- data.frame(
  run_timestamp = as.POSIXct(Sys.time(), tz = "UTC"),
  runner = Sys.info()[["nodename"]],
  working_directory = getwd(),
  stringsAsFactors = FALSE
)

print(result_df)
