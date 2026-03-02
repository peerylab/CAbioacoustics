
#' Write configuration parameters for PAM R project
#'
#' @param study_year Survey year
#' @param study_type Study type (e.g., Sierra Monitoring, Coastal Barred)
#' @param study_drive Drive label (e.g., Z:)
#' @param study_timezone Timezone of recordings
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' cb_config_pam_project(
#'   study_year = lubridate::year(Sys.Date()),
#'   study_type = 'Sierra_Monitoring',
#'   study_path = 'Z:',
#'   study_timezone = 'America/Los_angeles'
#' )
#' }

cb_config_pam_project <- function(study_year, study_type, study_drive, study_timezone) {

  # create config parameters
  config <-
    list(
      study_year = study_year,
      study_type = study_type,
      study_drive = study_drive,
      study_timezone = study_timezone
    )

  # create folder to store parameters
  fs::dir_create('config')

  # save to yaml
  yaml::write_yaml(
    config,
    here::here('config/project_config.yml')
  )

}
