
#' Extract date/datetime from a path or filename (`duckdb` translation)
#'
#' @param x
#' @param type
#'
#' @returns
#' @export
#'
#' @examples

cb_filename_to_datetime_duckdb <- function(x, type = c('datetime', 'date')) {

  # datetime
  if (type == 'datetime') {

    dbplyr::sql("
      strptime(
        regexp_replace(
          regexp_extract(x, '[0-9]{8}_[0-9]{6}'),
          '_',
          ''
        ),
        '%Y%m%d%H%M%S'
      )
    ")

    # date
  } else if (type == 'date') {

    lubridate::as_date(
      dbplyr::sql("
      strptime(
        regexp_replace(
          regexp_extract(x, '[0-9]{8}_[0-9]{6}'),
          '_',
          ''
        ),
        '%Y%m%d%H%M%S'
      )
    ")
    )

  }

}
