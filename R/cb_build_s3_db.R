
#' Create a database of UW-Madison S3 contents that is easy to query
#'
#' @param s3_df `Tibble` of S3 contents from `cb_get_s3_df`
#' @param db_path Location for storing `DuckDB`
#'
#' @return `DuckDB` containing S3 file paths in a table called `s3_contents`
#' @export
#'
#' @examples
#' \dontrun{
#' # build database
#' cb_build_s3_db(s3_df = s3_df, db_path = "Z:/Bioacoustics/s3_bucket_contents")
#' }

cb_build_s3_db <- function(s3_df, db_path) {

  # create database
  conn <- DBI::dbConnect(duckdb::duckdb(dbdir = stringr::str_glue('{db_path}/s3_files.duckdb')))

  # create empty table of species detections
  s3_template <-
    tibble::tibble(
      key = as.character(NA),
      last_modified = as.character(NA),
      e_tag = as.character(NA),
      size = as.numeric(NA),
      owner_id = as.character(NA),
      owner_display_name = as.character(NA),
      storage_class = as.character(NA),
      bucket = as.character(NA)
    )

  # create these tables in the database
  DBI::dbCreateTable(conn, "s3_contents", s3_template)
  DBI::dbAppendTable(conn, "s3_contents", s3_df)
  DBI::dbDisconnect(conn)

}
