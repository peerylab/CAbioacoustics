
#' Get x/y coordinates added as columns to an `sf` object
#'
#' @param df `sf` POINT object
#'
#' @return `sf` POINT object with columns for x/y coordinate values
#' @export
#'
#' @examples
#' \dontrun{
#' # create db connection
#' cb_connect_db()
#'
#' # read in 2024 sierra monitoring deployments
#' deployments_df <-
#'   tbl(conn, "acoustic_field_visits") |>
#'   filter(survey_year == 2024 & study_type == 'Sierra Monitoring') |>
#'   select(
#'     id,
#'     study_type,
#'     group_id,
#'     visit_id,
#'     cell_id,
#'     unit_number,
#'     deployment_name,
#'     survey_year,
#'     deploy_date,
#'     recover_date,
#'     utm_zone,
#'     utme,
#'     utmn
#'   ) |>
#'   # pull into memory
#'   collect()
#'
#' # disconnect now
#' cb_disconnect_db()
#'
#' # make ARUs sf, convert to WGS 84
#' deployments_sf <-
#'   deployments_df |>
#'   cb_make_aru_sf()
#'
#' # add coordinates
#' deployments_sf <-
#'   deployments_sf |>
#'   cb_get_coords()
#' }

cb_get_coords <- function(df) {

  df_coords <-
    df |>
    (\(x) dplyr::mutate(
      x,
      lon = sf::st_coordinates(x)[,1],
      lat = sf::st_coordinates(x)[,2]
    ))()

  return(df_coords)

}
