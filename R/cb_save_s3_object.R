
#' Save an object from S3 locally
#'
#' @param s3_key Key (i.e., file path) on S3
#' @param output_path Where to save the file locally
#'
#' @returns S3 object
#' @export
#'
#' @examples
#' \dontrun{
#' # connect to S3 database
#' cb_connect_s3_db(db_path = "Z:/Bioacoustics/Acoustic_Code/s3_bucket_contents")
#'
#' # pull a random S3 key
#' s3_key <-
#'   conn_s3 |>
#'     tbl('s3_contents') |>
#'     filter(regexp_matches(key, 'flac|json.gz')) |>
#'     slice_sample(n = 1) |>
#'     pull(key)
#'
#' # save it locally with the same file name
#' cb_save_s3_object(s3_key = s3_key, output_path = here::here(basename(s3_key)))
#' }

cb_save_s3_object <- function(s3_key, output_path) {

  aws.s3::save_object(
    object = s3_key,
    bucket = "mpeery-archive",
    file = output_path,
    region = "",
    base_url = "s3.drive.wisc.edu",
    key = keyring::key_get("aws_access_key_id"),
    secret = keyring::key_get("aws_secret_access_key")
  )

}
