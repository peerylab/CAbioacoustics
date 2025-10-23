
#' Disconnect from spotted owl database when finished with queries
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # disconnect from the owl database
#' cb_disconnect_db()
#' }

cb_disconnect_db <- function() {

  DBI::dbDisconnect(conn)

}
