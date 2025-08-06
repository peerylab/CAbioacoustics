
#' Get FLAC information from a hard drive
#'
#' @param drive_path
#' @param output_dir
#'
#' @return
#' @export
#'
#' @examples

cb_get_hd_contents <- function(drive_path, output_dir) {

  # flac file information
  df <-
    fs::file_info(
      fs::dir_ls(
        path = drive_path,
        recurse = TRUE,
        glob = '*.flac',
        # prevent this from breaking for RECYCLE.BIN folders, etc.
        fail = FALSE
      ) |>
        # get rid of RECYCLE files
        stringr::str_subset('RECYCLE', negate = TRUE)
    )

  # print volume label and number of flacs
  vol_label <- get_volume_label(stringr::str_remove(drive_path, '/'))
  message(stringr::str_glue('{vol_label}: {nrow(df)} flacs'))

  # write to output location with volume label
  df |>
    readr::write_csv(stringr::str_glue('{output_dir}/{vol_label}.csv'))

}
