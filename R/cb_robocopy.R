
#' Use `robocopy` to copy audio data from external hard drives
#'
#' @param source_drive Hard drive letter
#' @param destination Directory to copy hard drive files
#' @param threads Number of threads to use; for HDD may be better to use 8; SSD 16 or 32?
#' @param retries Number of times to retry copying a file if it fails
#' @param wait Time in seconds to wait for retry
#' @param log_dir Directory to store log files
#' @param exclude_recycle Exclude RECYCLE BIN files
#' @param verbose Print log output in console, or hide it
#'
#' @returns
#' @export
#'
#' @examples
#' \dontrun{
#' cb_robocopy(
#' source_drive = "D:",
#' destination = "C:/Users/jmwin/Documents/audio_data_backup",
#' threads = 16,
#' log_dir = "C:/Users/jmwin/Documents",
#' exclude_recycle = TRUE,
#' verbose = TRUE
#' )
#' }

cb_robocopy <- function(source_drive, destination, threads = 16, retries = 3, wait = 5, log_dir, exclude_recycle = TRUE, verbose = TRUE) {

  # extract drive letter to get volume label (name of hard drive)
  drive_letter <- sub("^(.:).*", "\\1", source_drive)
  label <- CAbioacoustics:::get_volume_label(drive_letter)

  # create destination folder
  destination <- file.path(destination, label)
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)

  # create log file folder and log file based on volume label
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  log_file <- file.path(log_dir, paste0(label, "_robocopy.log"))

  # set argument commands
  args <-
    c(
      shQuote(source_drive),
      shQuote(destination),
      "/E",
      "/Z",
      paste0("/R:", retries),
      paste0("/W:", wait),
      paste0("/MT:", threads),
      paste0('/LOG:', shQuote(log_file))
    )

  # show log output or not
  if (!verbose) {

    args <- c(args, "/NP", "/NFL", "/NDL")

  }

  # ignore RECYCLE BIN files
  if (exclude_recycle) {

    args <- c(args, "/XD", "$RECYCLE.BIN", "System Volume Information")

  }

  # run command
  message("Running robocopy: ", source_drive, " -> ", destination)

  system2("robocopy", args = args)

}
