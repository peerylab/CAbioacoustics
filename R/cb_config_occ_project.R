
#' Set parameters for an occupancy analysis
#'
#' @param start_year Start year
#' @param end_year End year
#' @param species Species (CSOW or BDOW)
#' @param template_used BirdNET version used
#' @param study_type Study type (e.g., Sierra Monitoring, Coastal Barred)
#' @param cell_list List of cell IDs to query (C0001, C0002, ...)
#'
#' @returns
#' @export
#'
#' @examples

cb_config_occ_project <- function(start_year, end_year, species, template_used, study_type, cell_list) {

  # create config parameters
  config <-
    list(
      start_year = start_year,
      end_year = end_year,
      species = species,
      template_used = template_used,
      study_type = study_type,
      cell_list = cell_list
    )

  # create folder to store parameters
  fs::dir_create('config')

  # save to yaml
  yaml::write_yaml(
    config,
    here::here('config/project_config.yml')
  )

}
