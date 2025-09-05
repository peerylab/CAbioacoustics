
#' Create expected FLAC file path based on drive location, survey year, and study regions
#'
#' @param network_drive
#' @param survey_year
#' @param regions
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # example ARU data frame
#' # set mapped drive to research path
#' network_drive <- 'Z:'
#' # path to data
#' path <- stringr::str_c(network_drive, "/Acoustic_Data/ARU_Data_Raw")
#' # current year = survey year
#' data_year <- lubridate::year(lubridate::today())

#' # string with proper naming convention
#' flac_file_structure <-
#'   cb_flac_file_structure(
#'     network_drive = network_drive,
#'     survey_year = data_year,
#'     regions = regions
#'  )
#'  print(flac_file_structure)
#' }

cb_flac_file_structure <- function(root) {

  regions <- basename(fs::dir_ls(root))

  flac_structure <-
    stringr::str_c(
      root,
      '/',
      '(',
      stringr::str_flatten(regions, '|'),
      ')/G(P|R|C|M|0)[0-9]{2}_V[1-5]{1}/G(P|R|C|M|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4}_U[1-5]{1}/G(P|R|C|M|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4}_U[1-5]{1}_[0-9]{8}/G(P|R|C|M|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4}_U[1-5]{1}_[0-9]{8}_[0-9]{6}Z.flac'
    )

  return(flac_structure)

}
