
#' Connect to database containing UW-Madison S3 contents
#'
#' @param db_path Path to directory containing S3 `DuckDB`
#'
#' @return `DuckDB` connection
#' @export
#'
#' @examples
#' \dontrun{
#' # connect to duckdb of S3 file paths
#' cb_connect_s3_db(db_path = "Z:/Bioacoustics/Acoustic_Code/s3_bucket_contents")
#' }

cb_connect_s3_db <- function(db_path) {

  # connect to local s3 database
  conn_s3 <<- DBI::dbConnect(duckdb::duckdb(dbdir = stringr::str_glue("{db_path}/s3_files.duckdb")))

}
