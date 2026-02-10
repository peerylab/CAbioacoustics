
#' Write configuration parameters for PAM R project
#'
#' @param study_year
#' @param study_type
#' @param study_path
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' cb_config_pam_project(
#'   study_year = lubridate::year(Sys.Date()),
#'   study_type = 'Sierra_Monitoring',
#'   study_path = 'Z:'
#' )
#' }

cb_config_pam_project <- function(study_year, study_type, study_path) {

  # create config parameters
  config <-
    list(
      study_year = study_year,
      study_type = study_type,
      study_path = study_path
    )

  # create folder to store parameters
  fs::dir_create('config')

  # save to yaml
  yaml::write_yaml(
    config,
    here::here('config/project_config.yml')
  )

}
