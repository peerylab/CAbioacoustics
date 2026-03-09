
#' Disconnect from S3 database
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # disconnect from S3 database
#' cb_disconnect_s3_db()
#' }

cb_disconnect_s3_db <- function() {

  DBI::dbDisconnect(conn_s3)

}
