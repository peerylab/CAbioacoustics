
#' Extract continuous raster data at the ARU-level (and within buffers around an ARU) or hex-level
#'
#' @param sf_object `sf` `POINT` object
#' @param scale aru or hex
#' @param raster_layer Path to raster layer
#' @param raster_name Name of raster (for naming raster covariate)
#' @param buffer_size Size in meters of buffers
#' @param fun Mean, SD, min, max, etc.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' library(CAbioacoustics)
#' library(tidyverse)
#' library(terra)
#' library(sf)
#'
#' # load canopy cover raster
#' # change path to research drive here if necessary
#' r_cfo_cc <- rast("Z:/CAbioacoustics_gis/cfo/cfo_canopy_cover_2020.tif")
#'
#' # create db connection
#' cb_connect_db()
#'
#' # read in 2024 sierra monitoring deployments
#' deployments_df <-
#'   tbl(conn, "acoustic_field_visits") |>
#'   filter(survey_year == 2024 & study_type == 'Sierra Monitoring') |>
#'   select(
#'   id,
#'   study_type,
#'   group_id,
#'   visit_id,
#'   cell_id,
#'   unit_number,
#'   deployment_name,
#'   survey_year,
#'   deploy_date,
#'   recover_date,
#'   utm_zone,
#'   utme,
#'   utmn
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
#' # get mean, SD of canopy cover in several buffers around ARUs
#' deployments_cc_buffer_sf <-
#'   cb_extract_raster(
#'     sf_object = deployments_sf,
#'     scale = 'aru',
#'     # CFO canopy cover raster
#'     raster_layer = r_cfo_cc,
#'     # name you want the covariate to have
#'     raster_name = 'cfo_cc',
#'     # buffer sizes in meters
#'     buffer_size = c(100, 500, 1000),
#'     # get mean, SD of canopy cover for different buffers
#'     fun = c('mean', 'stdev')
#'   )
#' }

cb_extract_raster <- function(sf_object, scale, raster_layer, raster_name, buffer_size = NULL, fun = NULL) {

  if (scale == 'aru' & is.null(buffer_size) == TRUE) {

    extract_df <- terra::extract(raster_layer, sf_object |> sf::st_transform(sf::st_crs(raster_layer)))

    sf_object <-
      sf_object |>
      dplyr::bind_cols(extract_df |> dplyr::select(2) |> dplyr::rename_at(1, ~ raster_name))

  }

  if (scale == 'aru' & is.null(buffer_size) == FALSE) {

    # get data frame with elevation summarized at different buffer sizes
    extract_df <-
      buffer_size %>%
      # create column names
      stringr::str_c(raster_name, ., sep = '_') |>
      purrr::set_names() |>
      # iterate through buffer sizes
      purrr::map_dfr(
        \(x)
        exactextractr::exact_extract(
          raster_layer,
          # buffer points
          sf_object |> sf::st_transform(sf::st_crs(raster_layer)) |> sf::st_buffer(as.numeric(stringr::str_extract(x, "\\d+"))),
          fun = fun,
          progress = FALSE
        ),
        .id = 'buffer_size'
      )

    if (length(fun) == 1) {

      # use single function name
      extract_df <-
        extract_df |>
        dplyr::select_all(list(~ paste0(fun, '_', .)))

    } else {

      # otherwise combine function, buffers for naming columns
      extract_df <-
        extract_df |>
        tidyr::pivot_longer(!buffer_size, names_to = "statistic", values_to = "value") |>
        tidyr::pivot_wider(
          names_from = c(statistic, buffer_size),
          names_glue = "{statistic}_{buffer_size}",
          values_from = value
        ) |>
        tidyr::unnest() |>
        suppressWarnings()

    }

    # bind to deployments
    sf_object <-
      sf_object |>
      dplyr::bind_cols(extract_df)

  }

  if (scale == 'hex' & is.null(buffer_size) == TRUE) {

    extract_df <-
      exactextractr::exact_extract(
        raster_layer,
        # buffer points
        sf_object |> sf::st_transform(sf::st_crs(raster_layer)),
        fun = fun,
        progress = FALSE
      ) |>
      tibble::as_tibble() |>
      dplyr::rename_at(1, ~ raster_name)

    # bind to deployments
    sf_object <-
      sf_object |>
      dplyr::bind_cols(extract_df)

  }

  return(sf_object)

}
