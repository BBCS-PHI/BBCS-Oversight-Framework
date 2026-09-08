make_sii_test_data <- function(
    aggregation_id = 151L,
    numerator = c(40, 45, 50, 55, 60),
    denominator = rep(100, 5),
    imd_code = 1:5
) {
  
  tibble::tibble(
    indicator_id = rep(10L, 5),
    start_date = rep(as.Date("2019-03-01"), 5),
    end_date = rep(as.Date("2020-02-29"), 5),
    numerator = numerator,
    denominator = denominator,
    indicator_value = rep(NA_real_, 5),
    lower_ci95 = rep(NA_real_, 5),
    upper_ci95 = rep(NA_real_, 5),
    imd_code = imd_code,
    aggregation_id = rep(aggregation_id, 5),
    age_group_code = rep(999L, 5),
    sex_code = rep(999L, 5),
    ethnicity_code = rep(999L, 5),
    creation_date = rep(
      as.POSIXct("2026-08-05 00:00:00", tz = "UTC"),
      5
    ),
    value_type_code = rep(14L, 5),
    source_code = rep(1L, 5),
    time_period_type = rep("12 month rolling", 5),
    combination_id = rep(1L, 5)
  )
}


testthat::test_that(
  "calculate_sii returns one correctly formatted result per grouping level",
  {
    
    input_data <- make_sii_test_data()
    
    result <- calculate_sii(input_data)
    
    testthat::expect_equal(nrow(result), 1L)
    
    testthat::expect_named(
      result,
      c(
        "indicator_id",
        "start_date",
        "end_date",
        "numerator",
        "denominator",
        "indicator_value",
        "lower_ci95",
        "upper_ci95",
        "imd_code",
        "aggregation_id",
        "age_group_code",
        "sex_code",
        "ethnicity_code",
        "creation_date",
        "value_type_code",
        "source_code",
        "time_period_type",
        "combination_id"
      )
    )
    
    testthat::expect_equal(result$numerator, 250)
    testthat::expect_equal(result$denominator, 500)
    
    testthat::expect_equal(
      result$indicator_value,
      25,
      tolerance = 1e-8
    )
    
    testthat::expect_true(is.na(result$lower_ci95))
    testthat::expect_true(is.na(result$upper_ci95))
    
    testthat::expect_equal(result$imd_code, 999L)
    testthat::expect_equal(result$indicator_id, 10L)
    testthat::expect_equal(result$aggregation_id, 151L)
    testthat::expect_equal(result$value_type_code, 14L)
    testthat::expect_equal(result$source_code, 1L)
    
    testthat::expect_equal(
      result$time_period_type,
      "12 month rolling"
    )
    
    testthat::expect_equal(result$combination_id, 1L)
  }
)


testthat::test_that(
  "calculate_sii rejects a grouping level with a missing quintile",
  {
    
    input_data <- make_sii_test_data() |>
      dplyr::filter(imd_code != 5)
    
    testthat::expect_error(
      calculate_sii(input_data),
      "do not contain exactly one row"
    )
  }
)


testthat::test_that(
  "calculate_sii rejects denominators equal to zero",
  {
    
    input_data <- make_sii_test_data(
      numerator = c(40, 45, 50, 55, 0),
      denominator = c(100, 100, 100, 100, 0)
    )
    
    testthat::expect_error(
      calculate_sii(input_data),
      "denominators must be greater than zero"
    )
  }
)


testthat::test_that(
  "calculate_sii returns one result for each grouping level",
  {
    
    group_one <- make_sii_test_data(
      aggregation_id = 151L
    )
    
    group_two <- make_sii_test_data(
      aggregation_id = 163L,
      numerator = c(35, 40, 45, 50, 55)
    )
    
    input_data <- dplyr::bind_rows(
      group_one,
      group_two
    )
    
    result <- calculate_sii(input_data)
    
    testthat::expect_equal(nrow(result), 2L)
    
    testthat::expect_setequal(
      result$aggregation_id,
      c(151L, 163L)
    )
    
    testthat::expect_true(
      all(result$imd_code == 999L)
    )
  }
)