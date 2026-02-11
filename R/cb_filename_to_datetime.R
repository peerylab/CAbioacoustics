
#' Extract date/datetime from a path or filename
#'
#' @param x Path or filename
#' @param type Datetime or date
#' @param time_zone Timezone of files
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
#' cb_filename_to_datetime(df$filename, 'datetime', timezone = 'America/Los_angeles')
#'
#' # or date
#' cb_filename_to_datetime(df$filename, 'date', timezone = 'America/Los_angeles')
#'
#' # or using dplyr::mutate()
#' df |>
#'   dplyr::mutate(date = cb_filename_to_datetime(filename, 'date', timezone = 'America/Los_angeles'))
#' }

cb_filename_to_datetime <- function(x, type = c('datetime', 'date'), timezone = 'America/Los_angeles') {

  # datetime
  if (type == 'datetime') {

    lubridate::ymd_hms(stringr::str_extract(x, '[0-9]{8}_[0-9]{6}'), tz = timezone)

  # date
  } else if (type == 'date') {

    lubridate::as_date(lubridate::ymd_hms(stringr::str_extract(x, '[0-9]{8}_[0-9]{6}'), tz = timezone))

  }

}
