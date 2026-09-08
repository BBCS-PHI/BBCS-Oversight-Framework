project_dir <- normalizePath(
  file.path("..", ".."),
  winslash = "/",
  mustWork = TRUE
)

source(file.path(project_dir, "transformations.R"))
source(file.path(project_dir, "time_periods.R"))
source(file.path(project_dir, "calculations.R"))
source(file.path(project_dir, "data_quality.R"))