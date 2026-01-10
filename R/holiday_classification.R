library(lubridate)
library(dplyr)
library(purrr)
library(magrittr)

determine_us_date_classification <- function(
    date_input_scalar, 
    include_black_friday = TRUE, 
    include_christmas_eve = FALSE, 
    include_day_after_christmas = FALSE
) {
  
  # --- INTERNAL HELPER FUNCTIONS -------------------------------------------
  
  # Helper: Validates and parses scalar input into a Date object
  parse_and_validate_date_input <- function(raw_input) {
    if (length(raw_input) != 1) {
      stop("Error: Function requires a scalar (single) date input.", call. = FALSE)
    }
    
    # Attempt parsing with YMD order strictly, allowing various separators
    parsed_date <- lubridate::ymd(raw_input, quiet = TRUE)
    
    if (is.na(parsed_date)) {
      stop(sprintf("Error: Input '%s' could not be interpreted as a valid Y-M-D date.", raw_input), call. = FALSE)
    }
    
    return(parsed_date)
  }
  
  # Helper: Generates a comprehensive list of holidays for a given year context
  # We generate for the target year and target year + 1 to handle New Year's Eve observation shifts
  generate_federal_holiday_manifest <- function(target_year) {
    
    years_to_process <- c(target_year, target_year + 1)
    
    # -- 1. Fixed Date Holidays --
    # Define base dates
    fixed_holidays_definitions <- list(
      list(name = "New Year's Day", month = 1, day = 1, type = "HARD"),
      list(name = "Juneteenth", month = 6, day = 19, type = "SOFT"),
      list(name = "Independence Day", month = 7, day = 4, type = "HARD"),
      list(name = "Veterans Day", month = 11, day = 11, type = "SOFT"),
      list(name = "Christmas Day", month = 12, day = 25, type = "HARD")
    )
    
    fixed_holiday_dates <- purrr::map_dfr(years_to_process, function(y) {
      purrr::map_dfr(fixed_holidays_definitions, function(def) {
        actual_date <- lubridate::make_date(y, def$month, def$day)
        
        # Calculate Observed Rule: Sat -> Fri, Sun -> Mon
        wday_val <- lubridate::wday(actual_date, label = FALSE, week_start = 1) # 6=Sat, 7=Sun
        
        observed_date <- dplyr::case_when(
          wday_val == 6 ~ actual_date - days(1),
          wday_val == 7 ~ actual_date + days(1),
          TRUE ~ actual_date
        )
        
        # Return both actual and observed as valid holiday instances
        tibble::tibble(
          date = c(actual_date, observed_date),
          type = def$type,
          holiday_name = def$name
        )
      })
    }) %>% dplyr::distinct()
    
    # -- 2. Floating Date Holidays (Always Mon or Thu) --
    # Helper for Nth day of month
    get_nth_wday <- function(y, m, target_wday, n) {
      # target_wday: 1=Mon, 4=Thu
      start_of_month <- lubridate::make_date(y, m, 1)
      dates_in_month <- start_of_month + 0:30
      dates_in_month <- dates_in_month[month(dates_in_month) == m]
      valid_wdays <- dates_in_month[wday(dates_in_month, week_start = 1) == target_wday]
      
      if (n == "last") {
        return(tail(valid_wdays, 1))
      } else {
        return(valid_wdays[n])
      }
    }
    
    floating_holiday_dates <- purrr::map_dfr(years_to_process, function(y) {
      tibble::tribble(
        ~date, ~type, ~holiday_name,
        get_nth_wday(y, 1, 1, 3), "SOFT", "Martin Luther King, Jr. Day",
        get_nth_wday(y, 2, 1, 3), "SOFT", "Washington's Birthday",
        get_nth_wday(y, 5, 1, "last"), "HARD", "Memorial Day",
        get_nth_wday(y, 9, 1, 1), "HARD", "Labor Day",
        get_nth_wday(y, 10, 1, 2), "SOFT", "Columbus Day",
        get_nth_wday(y, 11, 4, 4), "HARD", "Thanksgiving Day"
      )
    })
    
    # -- 3. Optional "Smart" Soft Holidays --
    optional_holiday_dates <- tibble::tibble(date = as.Date(character()), type = character(), holiday_name = character())
    
    if (include_black_friday) {
      # Day after Thanksgiving (4th Thu + 1 day)
      bf_dates <- purrr::map_dfr(years_to_process, function(y) {
        tg_date <- get_nth_wday(y, 11, 4, 4)
        tibble::tibble(date = tg_date + days(1), type = "SOFT", holiday_name = "Black Friday")
      })
      optional_holiday_dates <- dplyr::bind_rows(optional_holiday_dates, bf_dates)
    }
    
    if (include_christmas_eve) {
      ce_dates <- purrr::map_dfr(years_to_process, function(y) {
        tibble::tibble(date = lubridate::make_date(y, 12, 24), type = "SOFT", holiday_name = "Christmas Eve")
      })
      optional_holiday_dates <- dplyr::bind_rows(optional_holiday_dates, ce_dates)
    }
    
    if (include_day_after_christmas) {
      dac_dates <- purrr::map_dfr(years_to_process, function(y) {
        tibble::tibble(date = lubridate::make_date(y, 12, 26), type = "SOFT", holiday_name = "Day After Christmas")
      })
      optional_holiday_dates <- dplyr::bind_rows(optional_holiday_dates, dac_dates)
    }
    
    # Combine all manifests
    final_manifest <- dplyr::bind_rows(fixed_holiday_dates, floating_holiday_dates, optional_holiday_dates) %>%
      dplyr::distinct(date, .keep_all = TRUE)
      
    return(final_manifest)
  }

  # --- MAIN EXECUTION FLOW -------------------------------------------------
  
  # 1. Parse Input
  target_date <- parse_and_validate_date_input(date_input_scalar)
  target_year <- lubridate::year(target_date)
  
  # 2. Generate Reference Data
  holiday_schedule <- generate_federal_holiday_manifest(target_year)
  
  # 3. Determine Attributes
  is_weekend <- lubridate::wday(target_date, week_start = 1) %in% c(6, 7) # 6=Sat, 7=Sun
  
  matched_holiday <- holiday_schedule %>% 
    dplyr::filter(date == target_date)
  
  is_holiday <- nrow(matched_holiday) > 0
  holiday_type <- if (is_holiday) matched_holiday$type[1] else NA_character_
  
  # 4. Classification Logic (Precedence Rules)
  classification <- dplyr::case_when(
    is_weekend & is_holiday ~ "HOLIWEEKEND",
    is_holiday ~ holiday_type, # Returns "HARD" or "SOFT"
    is_weekend ~ "WEEKEND",
    TRUE ~ "WORKDAY"
  )
  
  return(classification)
}
