
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

  df <-
    fs::file_info(
      fs::dir_ls(
        path = drive_path,
        recurse = TRUE,
        glob = '*.flac'
      )
    )

  hd_name <- stringr::str_extract(drive_path, '[]')

  df |>
    readr::write_csv(stringr::str_glue('{output_dir}/{hd_name}.csv'))

}
