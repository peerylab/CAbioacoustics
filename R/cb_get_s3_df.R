
#' Return a data frame of S3 bucket contents
#'
#' @param prefix Search a particular folder or not; default is NULL
#' @param include_folders Whether to include folder markers or not; default is FALSE
#'
#' @return A `tibble` of S3 keys and metadata
#' @export
#'
#' @examples
#' \dontrun{
#' # return tibble of S3 keys and metadata for one folder
#' nest_video_df <- cb_get_s3_df(prefix = "Nest_Video_Data/")
#'
#' # or the whole S3 bucket
#' s3_df <- cb_get_s3_df()
#' }

cb_get_s3_df <- function(prefix = NULL, include_folders = FALSE) {

  s3_df <-
    aws.s3::get_bucket_df(
      bucket = "mpeery-archive",
      prefix = prefix,
      region = "",
      base_url = "s3.drive.wisc.edu",
      key = keyring::key_get("aws_access_key_id"),
      secret = keyring::key_get("aws_secret_access_key"),
      max = Inf
    ) |>
    tibble::as_tibble() |>
    janitor::clean_names() |>
    dplyr::mutate(size = as.numeric(size))

  # remove folder markers unless explicitly requested
  if (include_folders == FALSE) {

    s3_df <-
      s3_df |>
      dplyr::filter(!grepl("/$", key))

  }

  return(s3_df)

}
