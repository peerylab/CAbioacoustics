
#' Create directories to store selection tables
#'
#' @param output Main directory to store selection tables
#' @param directory Directories to store selection tables by group and cell (e.g., G001/C0001)
#' @param species_group Make selection tables for CSOW/BDOW or forest owls
#'
#' @return Folders to hold selection tables
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

