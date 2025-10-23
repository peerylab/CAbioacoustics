
#' Delete record(s) from the database
#'
#' @param ids Database records to delete
#' @param table_name Name of database table
#'
#' @return Delete database records based on IDs
#' @export
#'
#' @examples
#' \dontrun{
#' # connect to database
#' cb_connect_db()
#'
#' # delete records in acoustic_field_visits
#' cb_delete_db_records(ids = c(1, 2, 3), table_name = 'acoustic_field_visits)
#'
#' # disconnect
#' cb_disconnect_db()
#' }

cb_delete_db_records <- function(ids, table_name) {

  # Convert the list of IDs to a string for the SQL query
  ids_to_delete <- paste(ids, collapse = ", ")

  # Create the DELETE SQL statement
  delete_query <- stringr::str_glue("DELETE FROM {table_name} WHERE id IN (", ids_to_delete, ")", sep = "")

  # Execute the DELETE query
  DBI::dbExecute(conn, delete_query)

}
