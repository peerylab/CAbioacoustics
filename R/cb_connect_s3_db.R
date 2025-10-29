
#' Connect to database containing UW-Madison S3 contents
#'
#' @return `duckdb` connection
#' @export
#'
#' @examples
#' \dontrun{
#' # connect to duckdb of S3 file paths
#' cb_connect_s3_db()
#' }

cb_connect_s3_db <- function() {

  # connect to local s3 database
  conn_s3 <<- DBI::dbConnect(duckdb::duckdb(dbdir = "Z:/s3_bucket_contents/s3_files.duckdb"))

}
