library(metricengineR)

cvd_data <- metricengineR::get_cvd_indicators(
  time_period_id = c(33, 32),
  system_level_id = c(1, 4, 5, 6, 7, 8)
)

head(cvd_data$data)
names(cvd_data$data)

cvdprevent::cvd_time_period_system_levels() |>
  janitor::clean_names() |>
  dplyr::filter(
    time_period_id %in% c(32, 33)
  ) |>
  dplyr::select(
    time_period_id,
    system_level_id,
    system_level_name
  ) |>
  dplyr::arrange(
    time_period_id,
    system_level_id
  )