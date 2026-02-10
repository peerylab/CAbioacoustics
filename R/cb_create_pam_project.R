
#' Create R project directory for processing PAM program data
#'
#' @return R project
#' @export
#'
#' @examples
#' \dontrun{
#' cb_create_pam_project(
#'   project_name = "2026_sierra_acoustic_data_processing",
#'   base_dir = "Z:/"
#' )
#' }


cb_create_pam_project <- function(project_name, base_dir = getwd()) {

  # folder paths
  proj_path <- fs::path(base_dir, project_name)

  main_dirs <- c(
    "R",
    "log_files",
    "outputs"
  )

  sub_dirs <- c(
    "01-pre_season",
    "02-field_season",
    "03-pre_birdnet",
    "04-post_birdnet"
  )

  message(glue("Creating project at: {proj_path}"))

  # create project
  if (!fs::dir_exists(proj_path)) {

    suppressWarnings(
      usethis::create_project(proj_path, open = FALSE)
    )

  } else {

    message("Project folder already exists!")

  }

  # create folder structure
  for (main in main_dirs) {
    for (sub in sub_dirs) {
      fs::dir_create(fs::path(proj_path, main, sub))
    }
  }

  message("✅ PAM project scaffold complete")

}
