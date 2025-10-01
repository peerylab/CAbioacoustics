
#' Get path info for JSONs on research drive
#'
#' @param root_path Path to JSON files
#'
#' @return Tibble of JSON file paths with size (MB) and file name
#' @export
#'
#' @examples

cb_get_json_paths <- function(root_path) {

  # Temporary output file for the listing
  tmp_csv <- tempfile(fileext = ".csv")

  # PowerShell command
  ps_cmd <-
    paste0(
      "powershell -Command \"",
      "Get-ChildItem -Path '", root_path, "' -Recurse -Include *.json.gz -File | ",
      "Select-Object @{Name='path';Expression={$_.FullName}}, ",
      "@{Name='bytes';Expression={$_.Length}} | ",
      "Export-Csv -Path '", tmp_csv, "' -NoTypeInformation\""
    )

  system(ps_cmd)

  # Read the csv straight into R
  json_file_info_df <-
    readr::read_csv(tmp_csv, show_col_types = FALSE) |>
    dplyr::mutate(
      path = stringr::str_replace_all(path, '\\\\', '/')
    )

  return(json_file_info_df)

}
