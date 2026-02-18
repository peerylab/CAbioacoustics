
#' Generate encounter histories and effort data for multi-state, multi-season (dynamic) occupancy analysis
#'
#' @param species Species (for now just CSOW)
#' @param template_used BirdNET version used
#' @param study_type Study type (e.g., Sierra Monitoring)
#' @param cell_ids Cells to query
#' @param start_year Start year
#' @param end_year End year
#' @param start_date Earliest date in the season to query
#' @param end_date Latest date in the season to query
#' @param occasion_length Length of secondary sampling occasion (in days)
#' @param num_occasions Number of secondary sampling occasions
#'
#' @returns List of three data frames containing encounter histories (long and wide format) and effort (wide format)
#' @export
#'
#' @examples
#' \dontrun{
#' start_year <- 2021
#' end_year <- 2025
#' study_type <- 'Sierra Monitoring'
#' species <- 'CSOW'
#' template_used <- 'SierraModel-1.0'
#'
#' # get list of cell IDs (this will get all usfs cells)
#' cell_list <- cb_cells_by_ownership('usfs')
#'
#' # determine start/end dates of primary time periods
#' aru_dates_df <-
#'   cb_season_dates(
#'     study_type = study,
#'     cell_ids = cell_list,
#'     start_year = start_year,
#'     end_year = end_year
#'     )
#'
#' # set occasion length (in days)
#' occasion_length <- 7
#'
#' # see if min/max dates work as is;
#' # otherwise this function will adjust the end date so there are N even length
#' # secondary sampling occasions
#' primary_season_dates_df <-
#'   cb_occ_start_end_dates(
#'     aru_dates_df = aru_dates_df,
#'     occasion_length = occasion_length
#'   )
#'
#' # now set these for querying the database and formatting encounter histories
#' start_date <- primary_season_dates_df$start_date |> str_sub(5, 10)
#' end_date <- primary_season_dates_df$end_date |> str_sub(5, 10)
#' num_occasions <- primary_season_dates_df$num_occasions
#'
#' # connect to database
#' cb_connect_db()
#'
#' # now query
#' occ_df <-
#'   cb_query_multi_state_occ(
#'     species = species,
#'     template_used = template_used,
#'     study_type = study_type,
#'     cell_ids = cell_list,
#'     start_year = start_year,
#'     end_year = end_year,
#'     start_date = start_date,
#'     end_date = end_date,
#'     occasion_length = occasion_length,
#'     num_occasions = num_occasions
#'   )
#'
#' # and disconnect
#' cb_disconnect_db()
#' }

cb_query_multi_state_occ <- function(species, template_used, study_type, cell_ids, start_year, end_year, start_date, end_date, occasion_length, num_occasions) {

  focal_species <- species
  study <- study_type
  template <- template_used
  min_year <- start_year
  max_year <- end_year

  # query acoustic field deployments table
  deployments_sql_df <-
    conn |>
    tbl('acoustic_field_visits') |>
    # keep necessary deployment data
    select(id:unit_number)

  # query acoustic efforts table
  efforts_sql_df <-
    conn |>
    tbl('acoustic_efforts') |>
    select(id:survey_hour) |>
    rename(effort_id = id)

  # query acoustic detections table
  detections_sql_df <-
    conn |>
    tbl('acoustic_detections') |>
    filter(species == focal_species & template_used == template) |>
    select(acoustic_effort_id, begin_file, species, call_type, sex) |>
    # use distinct survey hours; can have both MAN or BirdNET classifications for a single survey hour (which inflates survey effort)
    # only need a single detection per survey hour
    distinct()

  # combine efforts, deployment info, and detections
  efforts_deployments_detections_sql_df <-
    efforts_sql_df |>
    # deployment acoustic_field_visit_id = effort id; rename to join
    left_join(deployments_sql_df |> rename(acoustic_field_visit_id = id), by = 'acoustic_field_visit_id') |>
    # detections effort id = effort id
    left_join(detections_sql_df |> rename(effort_id = acoustic_effort_id), by = 'effort_id') |>
    # clean up
    select(survey_date:unit_number, sex, call_type)

  # keep relevant data for owls
  efforts_deployments_detections_sql_df <-
    efforts_deployments_detections_sql_df |>
    # make survey year an integer
    # mutate(survey_year = sql("cast(survey_year as signed)")) |>
    # filter to study type, years, and appropriate survey hours for owls; usfs cells only
    filter(
      # cell_id %in% cell_ids,
      study_type == study,
      survey_year >= min_year,
      survey_year <= max_year,
      # keep efforts from 8:00PM to 6:00AM (for owls)
      survey_hour >= '20:00' & survey_hour < '23:59' | survey_hour >= '00:00' & survey_hour < '05:59'
    ) |>
    # create a field for survey night (i.e., biologically-relevant night for an owl)
    mutate(
      survey_night = case_when(
        survey_hour >= '00:00' & survey_hour < '05:59' ~ sql("date_add(survey_date, interval -1 day)"),
        TRUE ~ survey_date
      )
    ) |>
    # discard survey date, just using survey_night now
    select(-survey_date)

  # effort data by day
  efforts_df <-
    efforts_deployments_detections_sql_df |>
    # now tally (count number of hours) per deployment, year, and survey night
    dplyr::group_by(cell_id, unit_number, survey_year, survey_night) |>
    dplyr::tally(name = 'survey_hours') |>
    # read into memory now
    dplyr::collect() |>
    # convert these to integers from integer64
    dplyr::mutate(
      # survey_year = as.integer(survey_year),
      survey_hours = as.integer(survey_hours)
    )

  # detections by day
  detections_df <-
    efforts_deployments_detections_sql_df |>
    dplyr::group_by(study_type, project_area, group_id, survey_year, cell_id, unit_number, survey_night) |>
    dplyr::summarise(
      n_male = sum(sex == "M", na.rm = TRUE),
      n_female = sum(sex == "F", na.rm = TRUE),
      n_both = sum(sex == "B", na.rm = TRUE),
      n_unknown = sum(sex == "U", na.rm = TRUE),
      n_total = dplyr::n(),  # total rows
      n_na = sum(is.na(sex)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      owl_status = dplyr::case_when(
        n_total == n_na ~ "no owls", # all rows are NA
        n_both > 0 | (n_male > 0 & n_female > 0) ~ "pair", # both sexes present
        TRUE ~ "single" # otherwise single
      )
    ) |>
    dplyr::collect()

  # add detections to effort data
  efforts_detections_df <-
    efforts_df |>
    dplyr::left_join(detections_df, by = c('cell_id', 'unit_number', 'survey_year', 'survey_night')) |>
    # create a cell_unit field; fix empty project area
    dplyr::mutate(
      cell_unit = stringr::str_c(cell_id, '_', unit_number),
      project_area = dplyr::case_when(
        project_area == "" ~ as.character(NA),
        TRUE ~ project_area
      )
    ) |>
    # organize
    dplyr::select(
      group_id, cell_id, unit_number, cell_unit, study_type, project_area,
      survey_year, survey_night, survey_hours, owl_status
    )

  # fill in missing dates with complete_survey_nights(); do this in parallel with furrr/future
  future::plan(future::multisession, workers = parallelly::availableCores() - 1)
  efforts_detections_df <-
    efforts_detections_df |>
    # complete these by splitting into cell units and year
    dplyr::group_split(cell_unit, survey_year) |>
    # fill in missing dates from season start to end date
    furrr::future_map_dfr(\(df) CAbioacoustics:::complete_survey_nights(df, start_date, end_date))

  # create data frame of occasion number and date
  occ_dates_df <-
    CAbioacoustics:::create_season_dates(
      start_year = start_year,
      end_year = end_year,
      start_date = start_date,
      end_date = end_date
    ) |>
    dplyr::group_by(survey_year) |>
    # subtract 1 so first day is 0
    dplyr::mutate(day_of_season = dplyr::row_number() - 1) |>
    # split season into equal intervals (1-day, 3-day, ...1 week, etc)
    # ntile() assigns each day into a sampling occasion
    dplyr::mutate(
      occasion = dplyr::ntile(day_of_season, num_occasions)
    ) |>
    dplyr::ungroup()

  # assign secondary sampling occasion to each date
  efforts_detections_df <-
    efforts_detections_df |>
    # dplyr::left_join(occ_dates_df, by = dplyr::join_by('survey_year', 'survey_night'))
    dplyr::left_join(occ_dates_df, by = c('survey_year', 'survey_night'))

  # 1) summarize mean date over sampling occasion (days since start date)
  dates_df <-
    occ_dates_df |>
    dplyr::group_by(survey_year, occasion) |>
    dplyr::summarise(
      mean_date = mean(lubridate::yday(survey_night))
    ) |>
    suppressMessages() |>
    dplyr::ungroup()

  # 2) summarize maximum number of ARUs operational over a sampling occasion
  active_arus_df <-
    efforts_detections_df |>
    dplyr::filter(survey_hours > 0) |>
    dplyr::group_by(cell_id, survey_year, survey_night) |>
    dplyr::tally(name = 'arus') |>
    # dplyr::left_join(occ_dates_df |> dplyr::select(-day_of_season), by = dplyr::join_by('survey_year', 'survey_night')) |>
    dplyr::left_join(occ_dates_df |> dplyr::select(-day_of_season), by = c('survey_year', 'survey_night')) |>
    dplyr::group_by(cell_id, survey_year, occasion) |>
    dplyr::summarise(
      arus = max(arus)
    ) |>
    suppressMessages() |>
    dplyr::ungroup()

  # 3) summarize total survey hours over a sampling occasion
  survey_hours_df <-
    efforts_detections_df |>
    dplyr::group_by(cell_id, survey_year, occasion) |>
    dplyr::summarise(
      survey_hours = sum(survey_hours)
    ) |>
    suppressMessages() |>
    dplyr::ungroup() |>
    # 0s here are occasions where there were no arus deployed
    dplyr::mutate(
      survey_hours = dplyr::case_when(
        survey_hours == 0 ~ as.numeric(NA),
        TRUE ~ survey_hours
      )
    )

  # now combine with detections and summarise by cell, survey year, and occasion
  encounter_histories_long_df <-
    efforts_detections_df |>
    dplyr::group_by(cell_id, survey_year, occasion) |>
    dplyr::summarise(
      owl_status = dplyr::case_when(
        all(is.na(owl_status)) ~ as.character(NA), # NA; unsurveyed day
        all(owl_status == "no owls") ~ "no owls", # all days empty
        any(owl_status == "pair") ~ "pair", # any day has a pair
        all(is.na(owl_status) | owl_status == "no owls") ~ "no owls", # either NA or no owls should be no owls
        TRUE ~ "single" # otherwise single
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      detection = dplyr::case_when(
        is.na(owl_status) ~ as.numeric(NA),
        owl_status == "no owls" ~ 0,
        owl_status == "single" ~ 1,
        TRUE ~ 2
      )
    ) |>
    # join in various effort data frames
    dplyr::left_join(active_arus_df, by = c('cell_id', 'survey_year', 'occasion')) |>
    dplyr::left_join(survey_hours_df, by = c('cell_id', 'survey_year', 'occasion')) |>
    dplyr::left_join(dates_df, by = c('survey_year', 'occasion')) |>
    # organize
    dplyr::select(cell_id, survey_year, occasion, survey_hours, arus, mean_date, owl_status, detection)

  # calculate naive occupancy (across all occasions per site) while at it
  encounter_histories_long_df <-
    encounter_histories_long_df |>
    dplyr::group_by(cell_id, survey_year) |>
    dplyr::mutate(detection_naive = max(detection, na.rm = TRUE)) |>
    dplyr::ungroup()

  # convert dates to NA if there is no sampling
  encounter_histories_long_df <-
    encounter_histories_long_df |>
    dplyr::mutate(
      mean_date = dplyr::case_when(
        is.na(survey_hours) ~ as.numeric(NA),
        TRUE ~ mean_date
      )
    )

  # convert encounter histories to wide format
  encounters_dynamic_df <-
    encounter_histories_long_df |>
    dplyr::select(cell_id, survey_year, occasion, detection) |>
    tidyr::pivot_wider(
      id_cols = c('cell_id'),
      names_from = c(survey_year, occasion),
      names_glue = "{'det'}_{survey_year}_{occasion}",
      values_from = c(detection)
    )

  # convert effort to wide format
  effort_dynamic_df <-
    encounter_histories_long_df |>
    dplyr::select(cell_id, survey_year, occasion, survey_hours, arus, mean_date) |>
    tidyr::pivot_longer(
      !c(cell_id, survey_year, occasion),
      names_to = 'variable',
      values_to = 'value'
    ) |>
    tidyr::pivot_wider(
      id_cols = c('cell_id'),
      names_from = c(variable, survey_year, occasion),
      names_glue = "{variable}_{survey_year}_{occasion}",
      values_from = c(value)
    ) |>
    # fix order
    dplyr::select(cell_id, dplyr::matches('aru'), dplyr::matches('survey_hours'), dplyr::matches('mean_date')) |>
    dplyr::ungroup()

  # bundle up encounter histories (long and wide format) and effort data (wide format)
  # wide format is suitable for unmarked/ubms
  occ_df <-
    list(
      encounters_long = encounter_histories_long_df,
      encounters_wide = encounters_dynamic_df,
      effort_wide = effort_dynamic_df
    )

}
