
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
        glob = '*.flac'
      )
    )

  # name of hard drive (using groups as names here for now)
  hd_name <-
    stringr::str_flatten(
      basename(
        fs::dir_ls(
          drive_path,
          type = 'directory'
        )
      ),
      collapse = '_'
    )

  # write to output location
  df |>
    readr::write_csv(stringr::str_glue('{output_dir}/{hd_name}.csv'))

}
