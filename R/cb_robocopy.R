
#' Use `robocopy` to copy audio data from external hard drives
#'
#' @param source
#' @param destination
#' @param threads
#' @param retries
#' @param wait
#' @param log_file
#' @param show_output
#'
#' @returns
#' @export
#'
#' @examples

cb_robocopy <- function(source, destination, threads = 16, retries = 3, wait = 5, log_file, show_output = TRUE) {

  if (is.null(log_file)) {

    log_file <- file.path(destination, "robocopy_log.txt")

  }

  cmd <-
    sprintf(
      'robocopy "%s" "%s" /E /Z /R:%d /W:%d /MT:%d /TEE /LOG:"%s"',
      source,
      destination,
      retries,
      wait,
      threads,
      log_file
    )

  if (show_output) {

    message("Running:\n", cmd)

  }

  system(cmd, intern = FALSE)

}
