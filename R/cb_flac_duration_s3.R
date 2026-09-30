
#' Get duration of FLAC files (in seconds) stored on S3
#'
#' @param object S3 key
#' @param metadata If FALSE, return just FLAC file duration
#'
#' @returns
#' @export
#'
#' @examples

cb_flac_duration_s3 <- function(object, metadata = FALSE) {

  tmp <- tempfile(fileext = ".flac")
  on.exit(unlink(tmp), add = TRUE)

  system2(
    "aws",
    c(
      "s3api", "get-object",
      "--bucket", "mpeery-archive",
      "--key", object,
      "--range", "bytes=0-127",
      "--endpoint-url", "https://s3.drive.wisc.edu",
      "--no-verify-ssl",
      tmp
    ),
    stdout = FALSE,
    stderr = FALSE
  )

  hdr <- readBin(tmp, "raw", n = 128)

  stopifnot(rawToChar(hdr[1:4]) == "fLaC")

  packed <- hdr[19:26]

  bits <- paste(
    sapply(
      as.integer(packed),
      function(b) {
        paste(
          rev(as.integer(intToBits(b))[1:8]),
          collapse = ""
        )
      }
    ),
    collapse = ""
  )

  sample_rate <- strtoi(substr(bits, 1, 20), base = 2)
  channels <- strtoi(substr(bits, 21, 23), base = 2) + 1
  bits_per_sample <- strtoi(substr(bits, 24, 28), base = 2) + 1
  total_samples <- strtoi(substr(bits, 29, 64), base = 2)
  duration <- round(total_samples / sample_rate)

  if (metadata) {
    return(
      list(
        sample_rate = sample_rate,
        channels = channels,
        bits_per_sample = bits_per_sample,
        total_samples = total_samples,
        duration = duration
      )
    )
  }

  duration

}
