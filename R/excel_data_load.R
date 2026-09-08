library(readxl)
library(tidyverse)
library(purrr)

# Purpose:
# To load the Excel metrics data in a standardised format into [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_SQL_Staging_Data_Excel]
# The data are then inserted into [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_SQL_Staging_Data]
# The data are finally deduplicated
# Input file path: //Mlcsu-bi-fs/bsolccg/Reports/02_Routine/BBCS Oversight Framework/SQL scripts/Excel Input

# List all Excel files 
excel_files <- list.files(
  path = "//Mlcsu-bi-fs/bsolccg/Reports/02_Routine/BBCS Oversight Framework/SQL scripts/Excel Input",
  pattern = "\\.(xlsx|xlsm|xls)$",
  full.names = TRUE,
  ignore.case = TRUE
)

# Don't read the Data Input Template
excel_files <- excel_files[basename(excel_files) != "Data Input Template.xlsx"]

# Establish SQL connection
sql_connection <- dbConnect(
  odbc::odbc(),
  Driver   = "SQL Server",
  Server   = "MLCSU-BI-SQL",
  Database = "Cluster_BBCS",
  Trusted_Connection = "Yes"
)

# Function to read an Excel file
read_excel_file <- function(file_path, sheet_name) {

  file_name <- basename(file_path)
  message("Processing file: ", file_name)

  tryCatch(
    {
      df <- readxl::read_excel(
        path = file_path,
        sheet = sheet_name
      ) |>
        dplyr::mutate(source_file = file_name)

      message("Excel file processed \u2705 ")
      
      return(df)
    },
    error = function(e) {
      warning(
        "Could not process ", file_name,
        ": ", conditionMessage(e)
      )

      NULL
    }
  )
}

# Read all Excel files
all_data <- purrr::map_dfr(
  excel_files,
  read_excel_file,
  sheet_name = "Data Input"
)

head(all_data)

# Append data to Oversight_Framework_Fact_SQL_Staging_Data_Excel
DBI::dbWriteTable(
  conn = sql_connection,
  name = DBI::Id(
    schema = "BBCS",
    table = "Oversight_Framework_Fact_SQL_Staging_Data_Excel"
  ),
  value = all_data,
  append = TRUE
)

# Insert data [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_SQL_Staging_Data]
dbExecute(sql_connection,
          "  INSERT INTO [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_SQL_Staging_Data] (
       [Reference_ID]
      ,[Start_Date]
      ,[End_Date]
      ,[Provider_Code]
      ,[Provider_Site_Code]
      ,[ICB]
      ,[Age]
      ,[Numerator]
      ,[Denominator]
      ,[Ethnicity_Code]
      ,[IMD_Quintile]
      )
	  (
  SELECT [Reference_ID]
      ,[Start_Date]
      ,[End_Date]
      ,[Provider_Code]
      ,[Provider_Site_Code]
      ,[ICB]
      ,[Age]
      ,[Numerator]
      ,[Denominator]
      ,[Ethnicity_Code]
      ,[IMD_Quintile]
	  FROM [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_SQL_Staging_Data_Excel]
	  ) "
)

# Remove the Excel staging table from the database
dbExecute(sql_connection,
          "DROP TABLE IF EXISTS [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_SQL_Staging_Data_Excel]")

# Deduplicate data
dbExecute(
  sql_connection,
  "DROP TABLE IF EXISTS #Duplicates

 SELECT PK_ID
           ,ROW_NUMBER() OVER
           (
               PARTITION BY
                   Reference_ID,
                   Start_Date,
                   End_Date,
                   Provider_Code,
                   Provider_Site_Code,
                   ICB,
                   Age,
                   Ethnicity_Code,
                   IMD_Quintile
               ORDER BY PK_ID DESC
           ) AS rn
INTO #Duplicates
    FROM [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_SQL_Staging_Data]

  DELETE T1
    FROM [Cluster_BBCS].[BBCS].[Oversight_Framework_Fact_SQL_Staging_Data] T1
   INNER JOIN #Duplicates T2
      ON T1.PK_ID = T2.PK_ID
   WHERE T2.rn > 1"
)