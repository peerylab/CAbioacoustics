
#' Convert a data frame containing ARU UTM information into a simple feature object in WGS84
#'
#' @param df A data frame containing at minimum a column for the UTM zone (10 or 11; `utm_zone`) and easting (`utme`) and northing (`utmn`) coordinates
#'
#' @return A simple feature point object
#' @export
#'
#' @examples
#' \dontrun{
#' # example ARU data frame
#' aru_df <-
#' tibble::tribble(
#'   ~cell_id, ~utm_zone, ~utme, ~utmn,
#'   'C0001', 10, 670282, 4390557,
#'   'C0001', 10, 670972, 4390553,
#'   'C0001', 10, 669829, 4390388,
#'   'C0004', 11, 377972, 3919578
#' )
#' # convert to simple feature point geometry
#' arus_sf <-
#'   aru_df |>
#'     purrr::map_dfr(cb_make_aru_sf)
#'
#' # map
#' mapview::mapview(arus_sf)
#' }

cb_make_aru_sf <- function(df) {

  df <-
    df |>
    dplyr::mutate(
      crs = dplyr::case_when(
        utm_zone == 10 ~ 26910,
        utm_zone == 11 ~ 26911
      )
    ) |>
    split(~utm_zone) |>
    purrr::map_dfr(~ .x |>
                     sf::st_as_sf(coords = c("utme", "utmn"), crs = unique(.x$crs)) |>
                     sf::st_transform(4326)
    ) |>
    dplyr::select(-utm_zone, -crs)

  return(df)

}
