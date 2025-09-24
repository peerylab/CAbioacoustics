
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples

cb_flac_file_name <- function(df) {

  # groups
  group_df <-
    df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # store all IDs in a list-column
      group_ids_in_path = list(stringr::str_extract_all(path, "G(P|R|C|M|N|0)[0-9]{2}")[[1]]),
      # consistent if only one unique value
      group_id_error = length(unique(group_ids_in_path)) > 1
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-group_ids_in_path)

  # visits
  visit_df <-
    df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # store all IDs in a list-column
      visit_ids_in_path = list(stringr::str_extract_all(path, "V[1-5]{1}")[[1]]),
      # consistent if only one unique value
      visit_id_error = length(unique(visit_ids_in_path)) > 1
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-visit_ids_in_path)

  # cells
  cell_df <-
    df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # store all IDs in a list-column
      cell_ids_in_path = list(stringr::str_extract_all(path, "C[0-9]{4,5}")[[1]]),
      # consistent if only one unique value
      cell_id_error = length(unique(cell_ids_in_path)) > 1
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-cell_ids_in_path)

  # units
  unit_df <-
    df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # store all IDs in a list-column
      unit_ids_in_path = list(stringr::str_extract_all(path, "U[1-5]{1}")[[1]]),
      # consistent if only one unique value
      unit_id_error = length(unique(unit_ids_in_path)) > 1
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-unit_ids_in_path)

  # dates
  date_df <-
    df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      # store all IDs in a list-column
      date_ids_in_path = list(stringr::str_extract_all(path, "\\d{8}")[[1]]),
      # consistent if only one unique value
      date_id_error = length(unique(date_ids_in_path)) > 1
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-date_ids_in_path)

  # combine errors
  error_df <-
    df |>
    dplyr::left_join(group_df, by = join_by(path)) |>
    dplyr::left_join(visit_df, by = join_by(path)) |>
    dplyr::left_join(cell_df, by = join_by(path)) |>
    dplyr::left_join(unit_df, by = join_by(path)) |>
    dplyr::left_join(date_df, by = join_by(path))

  return(error_df)

}
