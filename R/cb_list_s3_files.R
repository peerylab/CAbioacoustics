
#' Get a list of S3 keys using AWS CLI
#'
#' @param bucket S3 bucket name
#' @param url S3 url
#' @param output_file Text file of S3 keys
#' @param prefix Check particular part of folder of a bucket
#' @param stderr Suppress messages or not (default is FALSE)
#'
#' @returns A `tibble` of S3 keys and metadata
#' @export
#'
#' @examples
#' \dontrun{
#' bucket <- "mpeery-archive"
#' url <- "https://s3.drive.wisc.edu"
#' output_file <- "C:/Users/jwiniarski/Desktop/s3_files.txt"
#'
#' # check entire bucket
#' cb_list_s3_files(
#'   bucket = bucket,
#'   url = url,
#'   output_file = output_file
#'   )
#'
#' # check part of bucket
#' cb_list_s3_files(
#'   bucket = bucket,
#'   url = url,
#'   prefix = 'Acoustic_Data/ARU_Data_Processed/BirdNET_Results_JSON/JSON_Sierra_Monitoring/2025',
#'   output_file = output_file
#'   )
#' }

cb_list_s3_files <- function(bucket, url, output_file, prefix = NULL, stderr = FALSE) {

  args <-
    c(
      "s3api", "list-objects-v2",
      "--bucket", bucket,
      "--endpoint-url", url,
      if (!is.null(prefix)) c("--prefix", prefix),
      "--query", "Contents[].[Key,Size]",
      "--output", "text",
      "--no-verify-ssl"
    )

  system2("aws", args, stdout = output_file, stderr = stderr)

  invisible(output_file)

}
