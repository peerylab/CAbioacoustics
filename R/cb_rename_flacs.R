
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

  # get list of directories/files to change
  flacs <- fs::dir_ls(dir, recurse = TRUE)

  # create new directories first
  dirname(flacs) |>
    unique() |>
    stringr::str_replace_all(pattern, replacement) |>
    fs::dir_create()

  # then move files (i.e., rename)
  fs::file_move(
    path = flacs,
    new_path = stringr::str_replace_all(flacs, pattern, replacement)
  )

}
