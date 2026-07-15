
#' Get duration of FLAC files stored on S3
#'
#' @param object
#'
#' @returns
#' @export
#'
#' @examples

cb_s3_flac_duration <- function(object) {

  hdr <- cb_get_flac_header(object)

  cb_flac_duration(hdr)$duration

}
