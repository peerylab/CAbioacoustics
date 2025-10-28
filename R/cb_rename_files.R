
#' Rename FLAC files with incorrect deployment information
#'
#' @param dir Directory where FLACs are kept (for example, folder containing all FLACs for an ARU deployment)
#' @param pattern String needing replacing
#' @param replacement String to replace it with
#'
#' @return Renamed FLAC files in renamed directories
#' @export
#'
#' @examples
#' \dontrun{
#' # correct G001_C0001_V1_U2 to G001_C0001_V1_U1
#' cb_rename_flacs(
#'   dir = 'path/to/deployments/G001_C0001_V1_U2',
#'   pattern = 'U2',
#'   replacement = 'U1'
#'   )
#' }

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
