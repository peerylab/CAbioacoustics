
#' Read GPX files from Garmin GPS units
#'
#' @param gpx_file Path to GPX file
#'
#' @return A \code{sf} \code{POINT} object with GPS point name and datetime
#' @export
#'
#' @examples
#' \dontrun{
#' # path to GPX file
#' gpx_path <- "Z:/2025_sierra_acoustic_data_processing/code/02-field_season/gpx_files/South_GPS_WPTS_0605_2025.gpx"
#'
#' # turn GPX into sf POINT object
#' gpx_sf <- cb_read_gpx(gpx_path)
#' }

cb_read_gpx <- function(gpx_file) {

  sf::st_read(gpx_file, layer = "waypoints") |>
    dplyr::select(name, time, geometry)

}
