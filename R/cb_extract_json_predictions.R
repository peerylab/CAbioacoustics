
#' Extract predictions from JSON files using species-specific threshold values
#'
#' @param json
#' @param threshold_df
#' @param predictions_output
#' @param log_output
#'
#' @return
#' @export
#'
#' @examples

cb_extract_json_predictions <- function(json, threshold_df, predictions_output, log_output) {

  # get name of file first for saving things later
  json_name <-
    stringr::str_remove(basename(json), '.json.gz')

  # get file date time and start hour recording to determine which species to extract predictions for
  json_date_time <-
    lubridate::ymd_hms(stringr::str_extract(json_name, '[0-9]{8}_[0-9]{6}'), tz = 'America/Los_Angeles')

  # hour
  rounded_json_start_time <-
    hms::as_hms(lubridate::round_date(json_date_time, 'hour'))

  # try reading and parsing JSON
  json_df <-
    tryCatch({
      jsonlite::fromJSON(json) |>
        tibble::enframe() |>
        dplyr::filter(name == 'detections') |>
        tidyr::unnest_longer(value) |>
        dplyr::select(-name) |>
        dplyr::rename(relative_time = value_id) |>
        tidyr::unnest(cols = c(value)) |>
        tidyr::unnest(cols = c(value))
    }, error = function(e) {
      message("Failed to parse: ", json, " — ", e$message)

      # Log to a failures file
      tibble::tibble(json = json, error = e$message) |>
        write.table(
          stringr::str_glue('{log_output}/json_failures.txt'),
          col.names = FALSE,
          row.names = FALSE,
          append = TRUE,
          sep = "\t",
          quote = TRUE
        )

      return(NULL)

    })

  if (is.null(json_df)) {

    return(NULL)

  }

  # get predictions if everything parsed above
  # and do so depending on the hour of the recording
  if (rounded_json_start_time %in% all_bird_hours) {

    # all birds (diurnal, forest owl, csow, bdow)
    json_df <-
      json_df |>
      dplyr::group_by(relative_time) |>
      dplyr::mutate(
        species_code = dplyr::row_number() - 1,
        relative_time = as.numeric(relative_time)
      ) |>
      dplyr::ungroup() |>
      dplyr::inner_join(
        threshold_df,
        by = dplyr::join_by('species_code')
      ) |>
      dplyr::filter(value >= logit_threshold)

  } else if (rounded_json_start_time %in% csow_bdow_forest_owl_hours) {

    # csow/bdow, forest owls
    json_df <-
      json_df |>
      dplyr::group_by(relative_time) |>
      dplyr::mutate(
        species_code = dplyr::row_number() - 1,
        relative_time = as.numeric(relative_time)
      ) |>
      dplyr::ungroup() |>
      dplyr::inner_join(
        threshold_df |> dplyr::filter(species_type %in% c('csow_bdow', 'forest_owl')),
        by = dplyr::join_by('species_code')
      ) |>
      dplyr::filter(value >= logit_threshold)

  } else if (rounded_json_start_time %in% diurnal_bird_forest_owl_hours) {

    # diurnal birds and forest owls
    json_df <-
      json_df |>
      dplyr::group_by(relative_time) |>
      dplyr::mutate(
        species_code = dplyr::row_number() - 1,
        relative_time = as.numeric(relative_time)
      ) |>
      dplyr::ungroup() |>
      dplyr::inner_join(
        threshold_df |> dplyr::filter(species_type %in% c('diurnal', 'forest_owl')),
        by = dplyr::join_by('species_code')
      ) |>
      dplyr::filter(value >= logit_threshold)

  } else if (rounded_json_start_time %in% diurnal_bird_hours) {

    # diurnal birds only
    json_df <-
      json_df |>
      dplyr::group_by(relative_time) |>
      dplyr::mutate(
        species_code = dplyr::row_number() - 1,
        relative_time = as.numeric(relative_time)
      ) |>
      dplyr::ungroup() |>
      dplyr::inner_join(
        threshold_df |> dplyr::filter(species_type == 'diurnal'),
        by = dplyr::join_by('species_code')
      ) |>
      dplyr::filter(value >= logit_threshold)

  }

  # keep any predictions above species-specific thresholds
  if (nrow(json_df) > 0) {

    # save to file based on json name
    json_df |>
      # keep highest csow score (if multiple are present per 3-second chunk)
      dplyr::group_by(scientific_name, relative_time) |>
      # value = birdnet logit
      dplyr::filter(value == max(value)) |>
      dplyr::ungroup() |>
      dplyr::mutate(json = json_name) |>
      dplyr::select(json, relative_time, species_code, common_name, birdnet_logit = value) |>
      arrow::write_parquet(stringr::str_glue('{predictions_output}/{json_name}_filtered.parquet'))

  }

  # log successfully processed JSON
  tibble::tibble(json = json, n = nrow(json_df)) |>
    write.table(
      stringr::str_glue('{log_output}/json_successes.txt'),
      col.names = FALSE,
      row.names = FALSE,
      append = TRUE
    )

}
