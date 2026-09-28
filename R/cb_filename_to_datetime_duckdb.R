
#' Extract date/datetime from a path or filename (`duckdb` translation)
#'
#' @param x
#' @param type
#'
#' @returns
#' @export
#'
#' @examples

cb_filename_to_datetime_duckdb <- function(x, type = c("datetime", "date")) {

  type <- match.arg(type)

  x <- rlang::as_name(rlang::ensym(x))

  datetime <-
    paste0(
      "strptime(",
      "NULLIF(",
      "regexp_replace(",
      "regexp_extract(", x, ", '[0-9]{8}_[0-9]{6}')",
      ", '_', ''",
      ")",
      ", ''",
      "), ",
      "'%Y%m%d%H%M%S'",
      ")"
    )

  if (type == "datetime") {

    dbplyr::sql(datetime)

  } else {

    dbplyr::sql(
      paste0("CAST(", datetime, " AS DATE)")
    )

  }

}
