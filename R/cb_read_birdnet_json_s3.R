
#' Read a BirdNET JSON file (from S3) in tidy format
#'
#' @param object_key Path to BirdNET JSON (json.gz) on S3
#'
#' @return `Tibble` form of BirdNET JSON output
#' @export
#'
#' @examples
#' \dontrun{
#' # key on S3 to example JSON
#' json_path <-
#'   "Acoustic_Data/ARU_Data_Processed/BirdNET_Results_JSON/JSON_Sierra_Monitoring/2025/North/G010_V1/G010_V1_C0088_U2/G010_V1_C0088_U2_20250508/G010_V1_C0088_U2_20250508_060002Z.json.gz"
#'
#' # read JSON
#' birdnet_df <- cb_read_birdnet_json_s3(json_path)
#' }

cb_read_birdnet_json_s3 <- function(object_key) {

  # download and decompress JSON from S3
  raw_obj <-
    aws.s3::get_object(
      object = object_key,
      bucket = "mpeery-archive",
      region = "",
      base_url = "s3.drive.wisc.edu",
      key = keyring::key_get("aws_access_key_id"),
      secret = keyring::key_get("aws_secret_access_key")
    )

  con <- gzcon(rawConnection(raw_obj))
  on.exit(close(con))

  # parse JSON into R list
  json_list <- jsonlite::fromJSON(con)

  # species labels
  species_df <-
    tibble::enframe(json_list) |>
    dplyr::filter(name == 'labels') |>
    tidyr::unnest(cols = c(value)) |>
    tidyr::unnest(cols = c(value)) |>
    dplyr::mutate(
      species_code = dplyr::row_number() - 1
    ) |>
    tidyr::separate(value, c("scientific_name", "common_name"), sep = '_') |>
    dplyr::select(-name)

  # detections and birdnet scores
  df <-
    tibble::enframe(json_list) |>
    dplyr::filter(name == 'detections') |>
    tidyr::unnest_longer(value) |>
    dplyr::rename(relative_time = value_id) |>
    tidyr::unnest(cols = c(value)) |>
    tidyr::unnest(cols = c(value)) |>
    dplyr::rename(birdnet_logit = value) |>
    dplyr::select(-name) |>
    dplyr::group_by(relative_time) |>
    dplyr::mutate(species_code = dplyr::row_number() - 1) |>
    dplyr::ungroup() |>
    dplyr::select(relative_time, species_code, birdnet_logit) |>
    # join in species codes
    dplyr::left_join(species_df, by = 'species_code') |>
    dplyr::mutate(birdnet_prediction = round(cb_logit_to_confidence(birdnet_logit), 3)) |>
    dplyr::arrange(relative_time, common_name) |>
    dplyr::select(relative_time, dplyr::matches('name'), dplyr::matches('birdnet')) |>
    dplyr::mutate(relative_time = as.numeric(relative_time)) |>
    dplyr::arrange(relative_time)

  return(df)

}
