# hello_commute_tracker.R
# Purpose: minimal non-interactive R script for GitHub Actions learning

cat("Hello from commute-tracker\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Working directory:", getwd(), "\n")

output_path <- file.path("data", "hello_output.txt")

writeLines(
  text = c(
    "This file was created by an automated R script.",
    paste("Created at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ),
  con = output_path
)

cat("Wrote file to:", output_path, "\n")
