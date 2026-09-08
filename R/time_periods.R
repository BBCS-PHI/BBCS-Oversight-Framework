stat_mode <- function(x) {
  tx <- table(x, useNA = "no")              # count frequency of each unique value
  if (!length(tx)) return(NA_character_)    # handle empty input gracefully
  names(tx)[which.max(tx)]                  # return the name (value) with max frequency
}


# Function to check which indicators requiring pooled data ---------------------
# Only data requiring Census population will need to be aggregated to create pooled-years

require_pooled_data <- function(metadata) {
  # Inputs:
  #   metadata   - a dataframe containing at least column `denominator_source` and `indicator_id`
  # Output:
  #   A vector of indicator ids requiring pooled data
  ids <- metadata |>
    filter(str_detect(tolower(denominator_source), "census")) |>
    distinct(indicator_id) |>
    pull(indicator_id)
  
  return(ids)
}

# Global keys to pool data by
POOL_KEYS <- c(
  "indicator_id","imd_code","aggregation_id",
  "age_group_code","sex_code","ethnicity_code",
  "creation_date","value_type_code","source_code","combination_id"
)

# Function to generate year series ---------------------------------------------
generate_year_series <- function(min_year, max_year, span_years = 3L) {
  stopifnot(is.numeric(min_year), is.numeric(max_year), is.numeric(span_years))
  min_year     <- as.integer(min_year)
  max_year     <- as.integer(max_year)
  window_years <- as.integer(span_years)
  if (window_years < 1L) stop("span_years must be >= 1")
  
  gap <- window_years - 1L
  if ((max_year - min_year) < gap) {
    return(data.frame(from = integer(0), to = integer(0), k = integer(0)))
  }
  
  starts <- seq.int(min_year, max_year - gap)
  data.frame(
    from = starts,
    to   = starts + gap,
    k    = window_years
  )
}

# Example
# generate_year_series(2014, 2024, 3)
#
# generate_year_series(2014, 2024, 5)

# Function to create pooled data -----------------------------------------------
# Inputs:
#   df  - data frame containing at least POOL_KEYS, `start_date`, `numerator`,
#         `denominator`, `period_type` ("Calendar"/"Financial"), and `population_type`
#   ks  - integer vector of window sizes to pool over (e.g., c(3, 5))
#   time_period_3yrs (optional) - data frame with columns `from`, `to` defining
#         explicit 3-year windows; if NULL, windows are auto-generated from data
#   time_period_5yrs (optional) - data frame with columns `from`, `to` defining
#         explicit 5-year windows; if NULL, windows are auto-generated from data
#
# Output:
#   A data frame containing only the 3- and/or 5-year pooled rows (no yearly rows),
#   where `numerator` and `denominator` are summed over each window, dates are set
#   from `from`/`to` according to `period_type`, and `time_period_type` is
#   "3 year pooled" or "5 year pooled".
#
# Notes/Assumptions:
#   - Only rows with `population_type == "Census"` are considered.
#   - One yearly record per POOL_KEYS ? year is constructed internally before pooling.
#   - If custom window tables are provided, they must have integer `from`/`to` years.


create_pooled_with_ranges <- function(df, ks = c(3,5),
                                      POOL_KEYS,
                                      time_period_3yrs = NULL,
                                      time_period_5yrs = NULL) {
  
  # Select only indicators which use Census as denominator source
  df <- df |> filter(population_type == "Census")
  
  # Group data by POOL KEYS and year
  yearly <- df |>
    mutate(period_year = lubridate::year(as.Date(start_date))) |>
    filter(period_type %in% c("Calendar","Financial"),
           !is.na(period_year)) |>
    group_by(across(all_of(POOL_KEYS)), period_type, period_year, ) |>
    summarise(
      numerator   = sum(numerator,   na.rm = TRUE),
      denominator = sum(denominator, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (nrow(yearly) == 0) return(slice(df, 0)) # empty in, empty out
  
  # Build range windows (use data span if not provided)
  miny <- min(yearly$period_year, na.rm = TRUE)
  maxy <- max(yearly$period_year, na.rm = TRUE)
  
  ranges_list <- list()
  if (3 %in% ks) {
    rng3 <- if (is.null(time_period_3yrs)) {
      # auto-generate from data
      tibble::tibble(from = seq.int(miny, maxy - 2), to = from + 2, k = 3L)
    } else {
      time_period_3yrs |>  transmute(from = as.integer(from), to = as.integer(to), k = 3L)
    }
    ranges_list <- c(ranges_list, list(rng3))
  }
  if (5 %in% ks) {
    rng5 <- if (is.null(time_period_5yrs)) {
      tibble(from = seq.int(miny, maxy - 4), to = from + 4, k = 5L)
    } else {
      time_period_5yrs |>  transmute(from = as.integer(from), to = as.integer(to), k = 5L)
    }
    ranges_list <- c(ranges_list, list(rng5))
  }
  ranges <- bind_rows(ranges_list)
  
  # If the ranges sit outside the data years, drop those that cant match
  ranges <- ranges |>  filter(from >= miny, to <= maxy)
  
  if (nrow(ranges) == 0) {
    return(df |>  slice(0))
  }
  
  # Non-equi join each yearly row to all windows that cover its year
  pooled <- yearly |>
    inner_join(ranges, by = join_by(period_year >= from, period_year <= to)) |>
    group_by(across(all_of(POOL_KEYS)), period_type, from, to, k) |>
    summarise(
      numerator   = sum(numerator,   na.rm = TRUE),
      denominator = sum(denominator, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      # Map window start/end to actual dates by period_type
      start_date = if_else(
        period_type == "Calendar",
        lubridate::make_date(from, 1, 1),
        lubridate::make_date(from, 4, 1)
      ),
      end_date = if_else(
        period_type == "Calendar",
        lubridate::make_date(to, 12, 31),
        lubridate::make_date(to + 1, 3, 31)
      ),
      time_period_type = paste0(k, " year pooled"),
      indicator_value = NA_real_, lower_ci95 = NA_real_, upper_ci95 = NA_real_
    ) |>
    transmute(
      indicator_id, start_date, end_date,
      numerator, denominator,
      indicator_value, lower_ci95, upper_ci95,
      imd_code, aggregation_id, age_group_code, sex_code, ethnicity_code,
      creation_date, value_type_code, source_code, time_period_type, combination_id
    )
  
  pooled
}

# Function to identify time period type ----------------------------------------

get_duration_label <- function(start_date, end_date) {
  
  # Inputs:
  #   start_date - a Date vector
  #   end_date   - a Date vector
  # Output:
  #   A character vector of duration labels, aligned with input length
  
  # helper to check if period matches monthly pattern
  is_n_months <- function(n){
    anniv <- start_date %m+% months(n)
    (end_date == anniv) | (end_date == anniv - days(1))
  }
  
  # helper to check if period matches exactly n years
  is_n_years <- function(n) {
    anniv <- start_date %m+% years(n)
    (end_date == anniv) | (end_date == anniv - days(1))
  }
  
  case_when(
    is_n_months(1) ~ "Monthly",
    is_n_months(3) ~ "Quarterly",
    is_n_years(1) ~ "1 year",
    is_n_years(3) ~ "3 year pooled",
    is_n_years(5) ~ "5 year pooled",
    TRUE ~ NA_character_
  )
}

# Function to exclude incomplete data ------------------------------------------
# Automatically exclude incomplete data cycles (e.g., financial, calendar, or other)
# based on the typical end date (mode month-day) and typical duration between start and end dates.
exclude_incomplete_data <- function(
    df,
    period_type_col, # column name: year type (e.g., "Calendar", "Financial", "Other")
    start_date_col, # column name: start date
    end_date_col, # column name: end date
    time_period_col = NULL, # optional column name for time period type (e.g., "1 year", "3 year pooled")
    indicator_col = NULL,     # optional column name for indicator (e.g., "indicator_id")
    current_date = Sys.Date(), # current date reference for detecting incomplete cycles
    enforce_duration = TRUE, # whether to filter by typical duration
    tolerance_days = 5L  # allowed deviation (days) from typical duration
) {
  # Capture column names for tidy evaluation
  period_type <- rlang::ensym(period_type_col)
  start_date <- rlang::ensym(start_date_col)
  end_date <- rlang::ensym(end_date_col)
  time_period <- if (!is.null(time_period_col)) rlang::ensym(time_period_col) else NULL
  indicator   <- if (!is.null(indicator_col)) rlang::ensym(indicator_col) else NULL
  
  # Standardize year type and time period text for grouping
  df2 <- df |>
    mutate(
      .row_id  = dplyr::row_number(), # row id for tracking
      yt_lower = stringr::str_to_lower(as.character(!!period_type)),
      tp_lower = if (!is.null(time_period)) stringr::str_to_lower(as.character(!!time_period)) else NA_character_
    )
  
  # Determine which columns to group by (year type + optional time period)
  group_keys <- c("yt_lower", if (!is.null(time_period)) "tp_lower" else NULL)
  
  meta <- df2 |>
    filter(!is.na(!!end_date), !!end_date <= current_date) |> # keep only past data
    mutate(
      mmdd = format(!!end_date, "%m-%d"),  # extract month-day part of end date
      duration_days = if_else(!is.na(!!start_date), as.numeric(!!end_date - !!start_date) + 1, NA_real_)
    ) |>
    group_by(across(all_of(group_keys))) |>
    summarise(
      end_mmdd = stat_mode(mmdd), # most common end month-day (e.g., 03-31)
      typical_duration = stats::median(duration_days, na.rm = TRUE), # median number of days per cycle
      .groups = "drop"
    )
  
  # Compute filtered result
  result <- df2 |>
    left_join(meta, by = group_keys) |>  # Join metadata back to the original data and filter incomplete periods
    mutate(
      end_mmdd = coalesce(end_mmdd, "12-31"), # Default to Dec 31 if no mode end date found
      latest_month_end = floor_date(current_date, "month") - days(1),
      # Construct this year's expected cycle end date
      cycle_end_this_year = as.Date(sprintf("%d-%s", lubridate::year(current_date), end_mmdd)),
      # If this years cycle hasnt ended yet, use last years end date
      latest_cycle_end = case_when(
        tp_lower == "monthly" ~ latest_month_end,
        cycle_end_this_year <= current_date ~ cycle_end_this_year,
        TRUE ~ as.Date(sprintf("%d-%s", lubridate::year(current_date) - 1L, end_mmdd))
      ),
      # latest_cycle_end = if_else(
      #   cycle_end_this_year <= current_date, cycle_end_this_year,
      #   as.Date(sprintf("%d-%s", lubridate::year(current_date) - 1L, end_mmdd))
      # ),
      # Calculate actual duration for each record
      duration_days = if_else(!is.na(!!start_date) & !is.na(!!end_date),
                              as.numeric(!!end_date - !!start_date) + 1, NA_real_),
      # Check if duration is within the expected tolerance range
      duration_ok = if_else(
        enforce_duration & !is.na(typical_duration) & !is.na(duration_days),
        abs(duration_days - typical_duration) <= tolerance_days,
        TRUE
      )
    ) |>
    # Keep only rows that end on or before the latest completed cycle and have valid duration
    filter(!is.na(!!end_date), !!end_date <= latest_cycle_end, duration_ok)
  
  # Determine excluded rows (anti-join by row id)
  excluded <- df2 |> dplyr::anti_join(result |> dplyr::select(.row_id), by = ".row_id")
  
  # Print summary of exclusions
  total_excluded <- nrow(excluded)
  total_rows     <- nrow(df)
  if (total_excluded == 0) {
    message("No rows were excluded (0/", total_rows, ").")
  } else {
    message("Excluded ", total_excluded, " row(s) out of ", total_rows, ".")
    
    # Show excluded rows and unique start and end dates
    print(head(excluded))
    print(paste0("unique start dates: (", unique(excluded$start_date)))
    print(paste0("unique end dates: (", unique(excluded$end_date)))
    print(unique(excluded$indicator_id))
    
    if (!is.null(indicator)) {
      by_indicator <- excluded |>
        dplyr::mutate(`Indicator` = as.character(!!indicator)) |>
        dplyr::count(`Indicator`, name = "Excluded_Rows") |>
        dplyr::arrange(dplyr::desc(Excluded_Rows))
      print(by_indicator)
    }
  }
  
  # Return filtered data without helper columns
  result |>
    dplyr::select(-yt_lower, -tp_lower, -cycle_end_this_year) |>
    dplyr::select(-.row_id)
}

