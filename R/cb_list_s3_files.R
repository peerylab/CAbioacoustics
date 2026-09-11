
#' Get a list of S3 keys using AWS CLI
#'
#' @param bucket S3 bucket name
#' @param url S3 url
#' @param output_file Text file of S3 keys
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
#' tictoc::tic()
#' cb_list_s3_files(
#'   bucket = bucket,
#'   url = url,
#'   output_file = output_file
#'   )
#' tictoc::toc()
#' }

cb_list_s3_files <- function(bucket, url, output_file, stderr = FALSE) {

  system2(
    "aws",
    c(
      "s3api", "list-objects-v2",
      "--bucket", bucket,
      "--endpoint-url", url,
      "--query", "Contents[].[Key,Size]",
      "--output", "text",
      "--no-verify-ssl"
    ),
    stdout = output_file,
    stderr = stderr
  )

  invisible(output_file)

}
