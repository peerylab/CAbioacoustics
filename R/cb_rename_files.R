
#' Rename FLACs, JSONs, and BirdNET selection tables (and associated directories) with incorrect deployment information
#'
#' @param dir Directory where files are kept (for example, folder containing all FLACs for an ARU deployment)
#' @param file_type .flac, .json.gz, or .BirdNET
#' @param pattern String needing replacing
#' @param replacement String to replace it with
#'
#' @return Renamed FLAC files in renamed directories
#' @export
#'
#' @examples
#' \dontrun{
#' # correct G001_C0001_V1_U2 to G001_C0001_V1_U1 for a set of FLACs
#' cb_rename_files(
#'   dir = 'path/to/deployments/G001_C0001_V1_U2',
#'   file_type = '.flac',
#'   pattern = 'U2',
#'   replacement = 'U1'
#'   )
#' }

cb_rename_files <- function(dir, file_type, pattern, replacement) {

  if (!file_type %in% c(".flac", ".json.gz", ".BirdNET")) {
    stop("Unsupported file type. Please provide a .flac, .json.gz, or .BirdNET file.")
  }

  # get list of directories to change
  dirs <- fs::dir_ls(dir, recurse = TRUE, type = 'directory')

  # get list of flacs to change
  files_to_rename <- fs::dir_ls(dir, recurse = TRUE, glob = stringr::str_glue('*{file_type}'))

  # create new directories first
  dirs |>
    stringr::str_replace_all(pattern, replacement) |>
    fs::dir_create()

  # then move files (i.e., rename)
  fs::file_move(
    path = files_to_rename,
    new_path = stringr::str_replace_all(files_to_rename, pattern, replacement)
  )

}
