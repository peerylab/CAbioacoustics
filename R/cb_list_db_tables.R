
#' List owl database tables
#'
#' @return Character vector of database table names
#' @export
#'
#' @examples
#' \dontrun{
#' cb_list_db_tables()
#' }

cb_list_db_tables <- function() {

  conn |>
    DBI::dbListTables()

}
