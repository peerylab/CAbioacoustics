
#' Get a list of S3 keys using AWS CLI
#'
#' @param bucket S3 bucket name
#' @param url S3 url
#' @param output_file Text file of S3 keys
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

cb_list_s3_files <- function(bucket, url, output_file) {

  system2(
    "aws",
    c(
      "s3api", "list-objects-v2",
      "--bucket", bucket,
      "--endpoint-url", url,
      "--query", "Contents[].[Key,Size]",
      "--output", "text"
    ),
    stdout = output_file
  )

  invisible(output_file)

}
