
#' Check for folders missing acoustic data on research drive
#'
#' @param root Directory of FLAC or JSON files
#'
#' @return Data frame of empty folder(s)
#' @export
#'
#' @examples
#' \dontrun{
#' # path to FLACs or JSONs
#' root <- "Z:/Acoustic_Data/ARU_Data_Raw/ARU_Sierra_Monitoring/2025"
#'
#' # check for empty folders (usually write this to a csv)
#' cb_check_empty_folders(root)
#' }

cb_check_empty_folders <- function(root) {

  # Temporary output file for the listing
  tmp_csv <- tempfile(fileext = ".csv")

  # PowerShell command
  ps_cmd <-
    paste0(
      "powershell -Command \"",
      "Get-ChildItem -Path '", root, "' -Recurse -Directory | ",
      "Where-Object { -Not (Get-ChildItem -Path $_.FullName -Recurse -Force -ErrorAction SilentlyContinue) } | ",
      "Select-Object @{Name='path';Expression={$_.FullName}} | ",
      "Export-Csv -Path '", tmp_csv, "' -NoTypeInformation\""
    )

  system(ps_cmd)

  # Read the csv straight into R
  empty_dirs_df <- readr::read_csv(tmp_csv, show_col_types = FALSE)

  # fix up paths if there are any empty directories
  if (dim(empty_dirs_df)[1] > 0) {

    empty_dirs_df <-
      empty_dirs_df |>
      mutate(path = stringr::str_replace_all(path, '\\\\', '/'))

  }

  return(empty_dirs_df)

}
