
#' Get duration of FLAC files stored on S3
#'
#' @param path
#'
#' @returns
#' @export
#'
#' @examples

cb_s3_flac_duration <- function(object) {

  tmp <- tempfile(fileext = ".flac")
  on.exit(unlink(tmp), add = TRUE)

  raw_obj <-
    aws.s3::get_object(
      object = object,
      bucket = "mpeery-archive",
      base_url = "s3.drive.wisc.edu",
      region = "",
      key = keyring::key_get("aws_access_key_id"),
      secret = keyring::key_get("aws_secret_access_key")
    )

  writeBin(raw_obj, tmp)

  av::av_media_info(tmp)$duration

}
