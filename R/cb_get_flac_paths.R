
#' Get path info for FLACs on research drive
#'
#' @param root_path Path to FLAC files
#'
#' @return Tibble containing FLAC paths and size of files (in bytes)
#' @export
#'
#' @examples
#' \dontrun{
#' # root path to scan
#' flac_root <- "Z:/Acoustic_Data/ARU_Data_Raw/ARU_Sierra_Monitoring/2025"
#'
#' # get df of flac info
#' flac_file_info_df <- cb_get_flac_paths(flac_root)
#' }

cb_get_flac_paths <- function(root_path) {

  # Temporary output file for the listing
  tmp_csv <- tempfile(fileext = ".csv")

  # PowerShell command
  ps_cmd <-
    paste0(
      "powershell -Command \"",
      "Get-ChildItem -Path '", root_path, "' -Recurse -Include *.flac -File | ",
      "Select-Object @{Name='path';Expression={$_.FullName}}, ",
      "@{Name='bytes';Expression={$_.Length}} | ",
      "Export-Csv -Path '", tmp_csv, "' -NoTypeInformation\""
    )

  system(ps_cmd)

  # Read the csv straight into R
  flac_file_info_df <-
    readr::read_csv(tmp_csv, show_col_types = FALSE) |>
    dplyr::mutate(
      path = stringr::str_replace_all(path, '\\\\', '/')
    )

  return(flac_file_info_df)

}
