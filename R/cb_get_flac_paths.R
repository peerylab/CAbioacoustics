
#' Get path info for FLACs on research drive
#'
#' @param root_path Path to FLAC files
#' @param deployment_string Deployment naming convention
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
#' flac_file_info_df <- cb_get_flac_paths(flac_root, "G(0|C|M|N|P|R)[0-9]{2}_V[1-5]{1}_C[0-9]{4}_U[1-5]{1}")
#' }

cb_get_flac_paths <- function(root_path, deployment_string) {

  # temporary output file to hold results
  tmp_csv <- tempfile(fileext = ".csv")

  # powerShell command
  ps_cmd <-
    paste0(
      "powershell -Command \"",
      "Get-ChildItem -Path '", root_path, "' -Recurse -Filter *.flac -File | ",
      "Select-Object @{Name='path';Expression={$_.FullName}}, ",
      "@{Name='bytes';Expression={$_.Length}} | ",
      "Export-Csv -Path '", tmp_csv, "' -NoTypeInformation\""
    )

  system(ps_cmd)

  # read in csv
  flac_file_info_df <-
    readr::read_csv(tmp_csv, show_col_types = FALSE) |>
    # clean up
    dplyr::transmute(
      path = stringr::str_replace_all(path, '\\\\', '/'),
      file_name = basename(path),
      deployment_name = stringr::str_extract(file_name, deployment_string),
      bytes = bytes
    ) |>
    # organize
    dplyr::select(deployment_name, path, file_name, bytes)

  return(flac_file_info_df)

}
