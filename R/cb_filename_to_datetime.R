
#' Extract date/datetime from a path or filename
#'
#' @param x Path or filename
#' @param type Datetime or date
#'
#' @return Datetime or date in PDT
#' @export
#'
#' @examples
#' \dontrun{
#' df <-
#'   tibble::tibble(
#'   filename = 'G001_V1_C0130_U2_20240527_060108Z.flac'
#'   )
#'
#' # get datetime
#' cb_filename_to_datetime(df$filename, 'datetime')
#'
#' # or date
#' cb_filename_to_datetime(df$filename, 'date')
#'
#' # or using dplyr::mutate()
#' df |>
#'   dplyr::mutate(date = cb_filename_to_datetime(filename, 'date'))
#' }

cb_filename_to_datetime <- function(x, type = c('datetime', 'date')) {

  if (type == 'datetime') {

    lubridate::ymd_hms(stringr::str_extract(x, '[0-9]{8}_[0-9]{6}'), tz = 'America/Los_angeles')

  } else if (type == 'date') {

    lubridate::as_date(lubridate::ymd_hms(stringr::str_extract(x, '[0-9]{8}_[0-9]{6}'), tz = 'America/Los_angeles'))

  }

}
