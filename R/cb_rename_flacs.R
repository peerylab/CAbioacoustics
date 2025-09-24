
#' Rename flacs files with incorrect deployment information
#'
#' @param dir
#' @param pattern
#' @param replacement
#'
#' @return
#' @export
#'
#' @examples

cb_rename_flacs <- function(dir, pattern, replacement) {

  # get list of directories to change
  dirs <- fs::dir_ls(dir, recurse = TRUE, type = 'directory')

  # get list of flacs to change
  flacs <- fs::dir_ls(dir, recurse = TRUE, glob = '*.flac')

  # create new directories first
  dirs |>
    stringr::str_replace_all(pattern, replacement) |>
    fs::dir_create()

  # then move files (i.e., rename)
  fs::file_move(
    path = flacs,
    new_path = stringr::str_replace_all(flacs, pattern, replacement)
  )

}
