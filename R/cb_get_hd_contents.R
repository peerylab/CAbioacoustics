
#' Get FLAC information from an external hard drive
#'
#' @param drive_path Path to external hard drive
#' @param output_dir Directory to save FLAC information
#'
#' @return `csv` containing FLAC file paths, sizes, etc.
#' @export
#'
#' @examples
#' \dontrun{
#' # get hard drive contents
#' cb_get_hd_contents('D:/', here::here())
#' }

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
    ) |>
    # make size a numeric value
    dplyr::mutate(size_num = as.numeric(size))

  # print volume label and number of flacs
  vol_label <- get_volume_label(stringr::str_remove(drive_path, '/'))
  message(stringr::str_glue('{vol_label}: {nrow(df)} flacs'))

  # write to output location with volume label
  df |>
    readr::write_csv(stringr::str_glue('{output_dir}/{vol_label}.csv'))

}
