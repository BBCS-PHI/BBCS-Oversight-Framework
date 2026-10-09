library(tidyverse)
library(readxl)
library(DBI)
library(odbc)
library(janitor)

cli::cli_h1("Loading metadata")

file_path <- "//Mlcsu-bi-fs/bsolccg/Reports/02_Routine/BBCS Oversight Framework/Metadata/"

file_name <- "NOF_26_27_Metrics_Metadata.xlsx"

connection_bsol <- dbConnect(
  odbc(),
  driver="SQL Server",
  server="MLCSU-BI-SQL",
  database="Cluster_BBCS",
  trusted_Connection="TRUE"
)

# Load in File -----------------------------------------------------------

indicator_data <- read_xlsx(
  file.path(paste0(file_path, file_name)), 
  sheet = "Metadata"
  ) |> 
  mutate(
    reference_id = as.character(reference_id)
    )


# Load in File -----------------------------------------------------------

derive_sql_data_types <- function(df, buffer = 0) {
  
  # Calculate max length by column
  max_lengths <- df %>%
    summarise(across(
      where(is.character),
      ~ max(nchar(., type = "bytes"), na.rm = TRUE)
    )) %>%
    as.list()
  
  # Map R types to SQL types
  r_to_sql <- list(
    character = function(name) {
      varchar_len <- max_lengths[[name]] + buffer
      sprintf("varchar(%s)", varchar_len)
    },
    integer = function(x) "int",
    numeric = function(x) "float",
    double  = function(x) "float",
    logical = function(x) "bit",
    Date    = function(x) "date",
    POSIXct = function(x) "datetime"
  )
  
  # Create output vector of column names and data types
  out <- map_chr(names(df), function(col) {
    col_class <- class(df[[col]])[1]
    mapper <- r_to_sql[[col_class]]
    
    if (is.null(mapper)) {
      warning(sprintf(
        "No SQL mapping defined for R class '%s'. Using varchar(max).",
        col_class
      ))
      return("varchar(max)")
    }
    
    mapper(col)
  })
  
  names(out) <- names(df)
  return(out)
}

# Load into SQL Table -----------------------------------------------------

dbExecute(connection_bsol,
          "DROP TABLE IF EXISTS [Cluster_BBCS].[BBCS].[Oversight_Framework_Reference_Metadata]"
)

dbWriteTable(
  connection_bsol,
  Id(schema = "BBCS", table = "Oversight_Framework_Reference_Metadata"),
  indicator_data,
  overwrite = TRUE,
  field.types = derive_sql_data_types(indicator_data)
)


dbExecute(connection_bsol,
          "DELETE FROM [Cluster_BBCS].[BBCS].[Oversight_Framework_Reference_Metadata] WHERE Reference_ID IS NULL"
)

dbExecute(connection_bsol,
          "Alter TABLE [Cluster_BBCS].[BBCS].[Oversight_Framework_Reference_Metadata]
           ALTER COLUMN Indicator_ID INT"
)

dbExecute(connection_bsol,
          " UPDATE [Cluster_BBCS].[BBCS].[Oversight_Framework_Reference_Metadata]
    SET indicator_id = T2.indicator_id
   FROM [Cluster_BBCS].[BBCS].[Oversight_Framework_Reference_Metadata] T1
  INNER JOIN [Cluster_BBCS].[BBCS].[Oversight_Framework_Reference_Indicator_List] T2
     ON T1.reference_id = t2.Reference_ID"
)

DBI::dbDisconnect(connection_bsol)

cli::cli_alert_success("Metadata loaded.")