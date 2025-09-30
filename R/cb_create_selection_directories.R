
#' Create directories to store selection tables
#'
#' @param date_time
#' @param directory
#'
#' @return
#' @export
#'
#' @examples

cb_create_selection_directories <- function(output, directory, species_group) {

  if (species_group != 'csow_bdow' & species_group != 'forest_owl') stop('must be `csow_bdow` or `forest_owl`')

  if (species_group == 'csow_bdow') {

    fs::dir_create(stringr::str_glue('{output}/{directory}'))

  } else if (species_group == 'forest_owl')

    fs::dir_create(stringr::str_glue('{output}/{directory}'))

}

