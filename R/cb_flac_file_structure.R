
#' Create expected FLAC file naming convention based on drive location, survey year, and study regions
#'
#' @param root Directory to FLAC files
#' @param study_type Study type (e.g., 'Sierra_Monitoring', 'NSO')
#'
#' @return String with proper FLAC naming convention and path structure
#' @export
#'
#' @examples
#' \dontrun{
#' # use this for string detection
#' flac_file_structure <- cb_flac_file_structure(root, study_type = 'Sierra_Monitoring')
#' print(flac_file_structure)
#' }

cb_flac_file_structure <- function(root, study_type) {

  if (study_type == 'Sierra_Monitoring') {

  regions <- basename(fs::dir_ls(root))

  flac_structure <-
    stringr::str_c(
      root,
      '/',
      '(',
      stringr::str_flatten(regions, '|'),
      ')/G(P|R|C|M|N|0)[0-9]{2}_V[1-5]{1}/G(P|R|C|M|N|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4,5}_U[1-5]{1}/G(P|R|C|M|N|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4,5}_U[1-5]{1}_[0-9]{8}/G(P|R|C|M|N|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4,5}_U[1-5]{1}_[0-9]{8}_[0-9]{6}Z.flac'
    )

  } else {

    flac_structure <-
      stringr::str_c(
        root,
        '/',
        'G(P|R|C|M|N|0)[0-9]{2}_V[1-5]{1}/G(P|R|C|M|N|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4,5}_U[1-5]{1}/G(P|R|C|M|N|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4,5}_U[1-5]{1}_[0-9]{8}/G(P|R|C|M|N|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4,5}_U[1-5]{1}_[0-9]{8}_[0-9]{6}Z.flac'
      )

  }

  return(flac_structure)

}
