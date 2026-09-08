# Purpose(s):
# To store all functions related to operations/transformations involving database


# Function to run SQL script ---------------------------------------------------
run_sql_file <- function(conn, path) {
  # Inputs:
  #   conn - SQL connection
  #   path - path to the SQL file
  #
  # Output:
  #   Prints a message indicating the SQL script has run successfully.
  sql_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  dbExecute(conn, sql_text)
  message("\u2705 SQL script has succesfully run!")
}

# Function to extract indicators from sql table --------------------------------
get_indicators_from_sql <- function(conn, table_name, indicator_ids = NULL) {
  # Base query
  sql_query <- paste0("SELECT * FROM ", table_name)
  
  # WHERE clause if IDs provided
  if (!is.null(indicator_ids) && length(indicator_ids) > 0) {
    # Quote each literal safely for this connection (handles numbers/strings)
    quoted <- vapply(indicator_ids, DBI::dbQuoteLiteral, character(1), conn = conn)
    sql_query <- paste0(sql_query, " WHERE Indicator_ID IN (", paste(quoted, collapse = ", "), ")")
  }
  
  # Execute
  result <- DBI::dbGetQuery(conn, sql_query)
  message("\u2705 Indicators extracted from SQL!")
  print(paste("Total rows for data extracted:", nrow(result)))
  result
}

# Function to insert data into SQL ---------------------------------------------
insert_data_into_sql_table <- function(conn, database, schema, table, data, indicator_ids = NULL,
                                       id_column = "indicator_id") {
  # Delete existing data and append new data
  # Inputs:
  #   conn          - Active SQL connection (e.g., from DBI::dbConnect)
  #   database      - Name of database (as a string)
  #   schema        - Name of schema (as a string)
  #   table         - Name of the SQL table (as a string)
  #   data          - Data frame to append to SQL
  #   indicator_ids - Optional vector of indicator IDs to delete before appending
  
  # Normalize the ids (NULL or "all"/"*" to delete all rows or certain ids)
  ids <- normalize_indicator_ids(indicator_ids)
  
  tbl_id  <- DBI::Id(catalog = database, schema = schema, table = table)
  tbl_sql <- DBI::dbQuoteIdentifier(conn, tbl_id)
  col_sql <- DBI::dbQuoteIdentifier(conn, id_column)
  
  # Build DELETE
  sql_query <- paste0("DELETE FROM ", tbl_sql)
  if (!is.null(ids) && length(ids) > 0) {
    # Quote each literal (works for numbers and strings)
    quoted_vals <- vapply(ids, DBI::dbQuoteLiteral, character(1), conn = conn)
    sql_query <- paste0(sql_query, " WHERE ", col_sql, " IN (", paste(quoted_vals, collapse = ", "), ")")
  } # else: NULL/empty then delete ALL rows in the table
  
  # Execute DELETE and append
  DBI::dbExecute(conn, sql_query)
  DBI::dbWriteTable(conn, name = tbl_id, value = data, append = TRUE)
  
  # Messages
  if (is.null(ids) || length(ids) == 0) {
    message("\u2705 All rows deleted and new data appended to ", DBI::dbQuoteIdentifier(conn, DBI::Id(schema=schema, table=table)), ".")
  } else {
    message("\u2705 Deleted and replaced data for selected indicator_ids in ", DBI::dbQuoteIdentifier(conn, DBI::Id(schema=schema, table=table)), ".")
  }
}

# Function to get age lookup from SQL ------------------------------------------
get_age_lookup_from_sql <- function(conn) {
  DBI::dbGetQuery(
    conn,
    "SELECT *
     FROM [Cluster_BBCS].[BBCS].[Metric_Engine_Reference_Age_Group]"
  )
}