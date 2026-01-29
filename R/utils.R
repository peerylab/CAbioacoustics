
# function to fill in missing survey days (diurnal bird outputs)
complete_survey_days <- function(df, start_date, end_date) {

  df |>
    # complete list of dates for each cell_unit by year
    tidyr::complete(
      deployment_name,
      date = seq.Date(
        lubridate::as_date(start_date),
        lubridate::as_date(end_date),
        by = "days"
      ),
      # fill in no effort
      fill = list(
        survey_hours = as.numeric(NA)
      )
    )

}


# function to fill in missing survey nights (i.e., nights with no effort)
complete_survey_nights <- function(df, start_date, end_date) {

  df |>
    # complete list of dates for each cell_unit by year
    tidyr::complete(
      group_id,
      cell_id,
      unit_number,
      cell_unit,
      study_type,
      project_area,
      survey_year,
      survey_night = seq.Date(
        lubridate::as_date(stringr::str_c(max(survey_year), start_date)),
        lubridate::as_date(stringr::str_c(max(survey_year), end_date)),
        by = "days"
      ),
      # fill in no effort
      fill = list(
        survey_hours = 0,
        detection = as.numeric(NA)
      )
    )

}


# create a date sequence given start/end dates
date_sequence <- function(df) {

  seq.Date(lubridate::as_date(df$start_date), lubridate::as_date(df$end_date), by = 'day')

}


# create a data frame of dates based off of start/end years and dates
create_season_dates <- function(start_year, end_year, start_date, end_date) {

  df <-
    tibble::tibble(
      survey_year = start_year:end_year,
      start_date = lubridate::as_date(stringr::str_c(survey_year, start_date)),
      end_date = lubridate::as_date(stringr::str_c(survey_year, end_date))
    )

  df <-
    df %>%
    dplyr::group_by(survey_year) %>%
    tidyr::nest() %>%
    dplyr::mutate(survey_night = purrr::map(data, date_sequence)) %>%
    tidyr::unnest(survey_night) %>%
    dplyr::ungroup() %>%
    dplyr::select(-data)

}


# get crs for utm zone
get_utm_crs <- function(df) {

  zone <- unique(df$utm_zone)

  if (zone == 10) {

    crs = 26910

  } else {

    crs = 26911

  }

  return(crs)

}


# hoots on same night as ARU data
hoots_same_night <- function(df, focal_date) {

  n_rows <-
    nrow(
      df |>
        # does it intersect ARU buffer?
        dplyr::filter(survey_night_date == lubridate::as_date(focal_date))
    )

  if (n_rows > 0) {

    hoots <- TRUE

  } else {

    hoots <- FALSE

  }

  return(hoots)

}


# find polygons intersecting another polygon
st_intersects_any <- function(x, y) {

  sf::st_intersects(x, y) %>%
    purrr::map_lgl(~length(.x) > 0)

}


# copy sd wav folders to desktop
copy_sd_wav_folders_to_desktop <- function(x, desktop_wav_path, sd_wav_folders, sd_card_path) {

  fs::dir_copy(
    path <- x,
    new_path <- stringr::str_c(desktop_wav_path, stringr::str_remove(path, sd_card_path), sep = '/'),
    overwrite = TRUE
  )

}


# convert flacs
flac_conversion <- function(desktop_wav_path, desktop_flac_path) {

  # create sox command
  command <-
    paste(
      desktop_wav_path,
      desktop_flac_path
    )

  # sox will then send this command over to the terminal for execution.
  seewave::sox(command, path2exe = "C:/Program Files (x86)/sox-14-4-2")

}


# copy desktop flacs to external hard drive
copy_to_hd <- function(desktop_flac_path, hard_drive_path, group_visit) {

  fs::dir_copy(
    path <-
      stringr::str_c(
        desktop_flac_path,
        group_visit,
        sep = '/'
      ),
    new_path <-
      stringr::str_c(
        hard_drive_path,
        # group_visit,
        sep = '/'
      ),
    overwrite = FALSE
  )

}


# currently not in use, but an updated version of read_birdnet_json
read_birdnet_json_2 <- function(json_gz_path, species_threshold_df, p_true_positive) {

  if (p_true_positive %ni% c(0.85, 0.9, 0.95, 0.975, 0.99)) stop("True positive probability must be either 0.85, 0.90, 0.95, 0.975, or 0.99")

  json_list <- jsonlite::fromJSON(json_gz_path)

  df <-
    tibble::enframe(json_list) |>
    dplyr::filter(name == 'detections') |>
    tidyr::unnest_longer(value) |>
    dplyr::rename(start_time = value_id) |>
    tidyr::unnest(cols = c(value)) |>
    tidyr::unnest(cols = c(value)) |>
    dplyr::rename(birdnet_logit = value) |>
    dplyr::select(-name) |>
    dplyr::group_by(start_time) |>
    dplyr::mutate(
      species_code = dplyr::row_number() - 1,
      start_time = as.integer(start_time),
      birdnet_confidence = logit_to_probability(birdnet_logit)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(start_time, species_code, birdnet_logit, birdnet_confidence) |>
    # if species, probability not null then filter by those
    dplyr::inner_join(species_threshold_df |> filter(p_tp == p_true_positive), by = join_by('species_code')) |>
    dplyr::filter(birdnet_logit >= logit_threshold) |>
    dplyr::arrange(start_time, species) |>
    dplyr::select(start_time, dplyr::matches('species'), birdnet_logit, birdnet_confidence, p_tp) |>
    dplyr::arrange(start_time, species_code)

}


# get file size
get_file_info <- function(path) {

  fs::file_info(
    fs::dir_ls(
      path
    )
  ) |>
    dplyr::select(path, size) |>
    dplyr::mutate(size = as.numeric(size))

}


# # get all daily subfolders
# get_subdirectory_contents <- function(df) {
#
#   subfolders <-
#     fs::dir_ls(
#       path = df$value,
#       recurse = 2
#     )
#
#   subfolder_length <- max(stringr::str_length(subfolders))
#
#   daily_subfolders <-
#     subfolders |>
#     tibble::as_tibble() |>
#     dplyr::filter(stringr::str_length(value) == subfolder_length) |>
#     dplyr::pull(value)
#
#   return(daily_subfolders)
#
# }


# # get folders/files for a directory
# get_files_next_depth <- function(path) {
#
#   df <-
#     fs::dir_ls(
#       path,
#       recurse = 0
#     ) |>
#     tibble::as_tibble() |>
#     dplyr::mutate(upper_level = path)
#
#   if (dim(df)[1] == 0) {
#
#     df <-
#       tibble::tibble(
#         value = NA,
#         upper_level = path
#       )
#
#   }
#
#   return(df)
#
# }


# move flacs to delete later (data proofing)
move_flacs_delete <- function(x) {

  fs::file_move(
    path = x,
    new_path = stringr::str_glue(here::here('flacs_to_delete/{basename(x)}'))
  )

}


# move jsons to delete later (data proofing)
move_jsons_delete <- function(x) {

  fs::file_move(
    path = x,
    new_path = stringr::str_glue(here::here('jsons_to_delete/{basename(x)}'))
  )

}


# # write pipeline files in digestible batches for the dtabase website
# write_pipeline_batches <- function(df, date_time) {
#
#   region <- unique(df$region)
#   study <- unique(df$study_type)
#   csv_num <- unique(df$csv_num)
#
#   df |>
#     dplyr::select(file_name) |>
#     write.table(
#       stringr::str_glue(here::here('code_outputs/effort_pipelines_{date_time}/{region}_{study}_{csv_num}_pipeline.csv')),
#       row.names = FALSE,
#       col.names = FALSE
#     )
#
# }


# delete a record from the database
delete_record <- function(table_name, record_id, con) {

  delete_query <- glue::glue_sql("DELETE FROM {table_name} WHERE (id = {record_id});", .con = con)
  print(delete_query)
  DBI::dbExecute(conn = con, statement = delete_query)

}


# # go from WGS84 to utm zone-specific crs
# wgs84_to_utm <- function(df) {
#
#   # get the crs depending on utm zone
#   zone <- unique(df$utm_zone)
#
#   if (zone == 10) {
#
#     aru_crs = 26910
#
#   } else {
#
#     aru_crs = 26911
#
#   }
#
#   # convert to utm zone-specific crs
#   df <-
#     df |>
#     sf::st_transform(aru_crs)
#
#   return(df)
#
# }


get_surveyor_overlap <- function(df, human_hoots) {

  # get birdnet file path and filename (XXXX.flac)
  focal_birdnet_path <- unique(df$filename)

  # create data frame with deployment name and survey night for each birdnet file
  birdnet_selection_df <-
    focal_birdnet_path |>
    tibble::as_tibble() |>
    dplyr::rename(begin_file = value) |>
    dplyr::mutate(
      # pull out date
      aru_date_time = lubridate::ymd_hms(stringr::str_extract(focal_birdnet_path, '[0-9]{8}_[0-9]{6}'), tz = 'America/Los_Angeles'),
      hour = hms::as_hms(lubridate::round_date(aru_date_time, 'hour'))
    ) |>
    dplyr::mutate(
      aru_survey_night = dplyr::case_when(
        hour %in% hms::parse_hms(c('00:00:00', '01:00:00', '02:00:00', '03:00:00', '04:00:00', '05:00:00')) ~ lubridate::as_date(aru_date_time) - 1,
        TRUE ~ lubridate::as_date(aru_date_time)
      )
    ) |>
    # just need deployment name and date for each birdnet file
    dplyr::transmute(
      deployment_name = stringr::str_extract(begin_file, 'G(P|R|C|M|0)[0-9]{2}_V[1-5]{1}_C[0-9]{4}_U[1-5]{1}'),
      aru_survey_night = aru_survey_night
    )

  # pull out deployment name separately (for querying database)
  focal_deployment <- birdnet_selection_df$deployment_name

  # create focal date
  focal_date <- birdnet_selection_df$aru_survey_night

  # now connect to the database
  cb_connect_db()

  # get coordinates and convert to ARU for unit matching deployment name
  focal_deployment_df <-
    conn |>
    dplyr::tbl('acoustic_field_visits') |>
    dplyr::select(deployment_name, survey_year, dplyr::matches('utm')) |>
    dplyr::filter(deployment_name == focal_deployment & survey_year == year(focal_date)) |>
    dplyr::collect()

  # close database connection
  cb_disconnect_db()

  # convert ARU deployment to sf with utms, utm zone
  # and buffer by 1.5 km to determine if there's surveyor overlap
  focal_deployment_sf <-
    focal_deployment_df |>
    dplyr::group_split(utm_zone) |>
    purrr::map_dfr(cb_make_aru_sf) |>
    sf::st_transform(3310) |>
    sf::st_buffer(dist = 1500)

  # now determine if there are any surveyor hoots on the same night as the birdnet detections
  hoots <- CAbioacoustics:::hoots_same_night(human_hoots, focal_date)

  # if there are, then see if any surveyor hoot locations intersect the ARU buffer;
  # otherwise set 'intersections' to FALSE
  if (hoots == TRUE) {

    focal_human_hoots_sf <-
      human_hoots %>%
      # does it intersect ARU buffer?
      dplyr::filter(survey_night_date == lubridate::as_date(birdnet_selection_df$aru_survey_night))

    intersections <-
      human_hoots %>%
      # does it intersect ARU buffer?
      dplyr::filter(survey_night_date == lubridate::as_date(birdnet_selection_df$aru_survey_night)) %>%
      dplyr::mutate(int = CAbioacoustics:::st_intersects_any(., focal_deployment_sf |> sf::st_transform(4326))) |>
      dplyr::filter(int == TRUE) |>
      dplyr::distinct(int) |>
      dplyr::pull()

  } else {

    intersections <- FALSE

  }

  # and now if there are surveyor hoots that overlap with the ARU buffer, set all of the
  # detections to 'surveyed' in overwrite column and save using that birdnet file's original path;
  # otherwise change overwrite to 'no' and save
  if (isTRUE(intersections) == TRUE) {

    df |>
      dplyr::select(-filename) |>
      dplyr::mutate(Overwrite = 'surveyed') |>
      dplyr::mutate(dplyr::across(c(View, `Begin File`, `Common Name`, Detector, Species, `Call Type`, Sex, Keep, Overwrite), ~tidyr::replace_na(., ""))) |>
      dplyr::arrange(Species, `Begin Time (s)`) |>
      write.table(
        file = focal_birdnet_path,
        sep = "\t",
        row.names = FALSE,
        col.names = TRUE,
        quote = FALSE
      )

  } else {

    df |>
      dplyr::select(-filename) |>
      dplyr::mutate(Overwrite = 'no') |>
      dplyr::mutate(dplyr::across(c(View, `Begin File`, `Common Name`, Detector, Species, `Call Type`, Sex, Keep, Overwrite), ~tidyr::replace_na(., ""))) |>
      dplyr::arrange(Species, `Begin Time (s)`) |>
      write.table(
        file = focal_birdnet_path,
        sep = "\t",
        row.names = FALSE,
        col.names = TRUE,
        quote = FALSE
      )

  }

}


# get duration of flac files
get_flac_duration <- function(file) {

  tryCatch({
    info <- av::av_media_info(file)
    info$duration  # duration in seconds
  }, error = function(e) NA_real_)

}


# selection table check function
check_unique_values <- function(df) {

  # see what unique entries are for each field
  cat('Detector\n', unique(df$Detector), '\n\n')
  cat('Species\n', unique(df$Species), '\n\n')
  cat('Call Type\n', unique(df$`Call Type`), '\n\n')
  cat('Sex\n', unique(df$Sex), '\n\n')
  cat('Keep\n', unique(df$Keep), '\n\n')
  cat('Overwrite\n', unique(df$Overwrite), '\n\n')

}


# shiny app functions -----------------------------------------------------

get_deployment_info <- function(sd_card_path, year) {

  # wav count
  n_wavs <- length(fs::dir_ls(sd_card_path, recurse = TRUE, glob = '*.wav'))

  # recording dates
  min_date <-
    fs::dir_ls(sd_card_path, recurse = TRUE, glob = '*.wav') |>
    stringr::str_extract('[0-9]{8}') |>
    lubridate::ymd() |>
    min()

  max_date <-
    fs::dir_ls(sd_card_path, recurse = TRUE, glob = '*.wav') |>
    stringr::str_extract('[0-9]{8}') |>
    lubridate::ymd() |>
    max()

  # deployment_name
  swift <-
    fs::dir_ls(sd_card_path) |>
    stringr::str_subset('S[0-9]{4}') |>
    head(1) |>
    stringr::str_extract('S[0-9]{4}')

  # current_year <- lubridate::year(lubridate::today())

  cb_connect_db()
  focal_deployment <-
    conn |>
    dplyr::tbl('acoustic_field_visits') |>
    dplyr::filter(swift_id == swift & survey_year == year) |>
    dplyr::collect() |>
    dplyr::pull(deployment_name)
  cb_disconnect_db()

  tibble::tibble(
    n_wavs = n_wavs,
    min_date = format(min_date, "%B %d"),
    max_date = format(max_date, "%B %d"),
    deployment_name = focal_deployment,
    swift_id = swift
  )

}

create_subfolders <- function(x, deployment_name, hard_drive_path) {

  fs::dir_create(
    stringr::str_glue('{hard_drive_path}/{stringr::str_extract(deployment_name, "G(P|R|C|M|0)[0-9]{2}_V[1-5]")}/{deployment_name}/{deployment_name}_{x}')
  )

}

# convert SD wavs to SSD flacs
wav_to_flac <- function(wav_path, deployment_name, desktop_path, hard_drive_path) {

  wav_date_time <- stringr::str_extract(wav_path, '[0-9]{8}_[0-9]{6}')
  wav_desktop_path <- stringr::str_glue('{desktop_path}/{basename(wav_path)}')

  fs::file_copy(
    wav_path,
    wav_desktop_path
  )

  flac_ssd_path <- stringr::str_glue('{hard_drive_path}/{stringr::str_extract(deployment_name, "G(P|R|C|M|0)[0-9]{2}_V[1-5]")}/{deployment_name}/{deployment_name}_{stringr::str_extract(wav_date_time, "[0-9]{8}")}/{deployment_name}_{wav_date_time}Z.flac')

  # wav to flac compression
  seewave::sox(
    stringr::str_glue('"{wav_desktop_path}" "{flac_ssd_path}"'),
    path2exe = "C:/Program Files (x86)/sox-14-4-2"
  )

  # delete temporary wav on desktop
  fs::file_delete(wav_desktop_path)

}


# convert SD wavs to SSD flacs (sequentially)
sequential_wav_to_flac <- function(wav_path, deployment_name, hard_drive_path) {

  # wav date and time
  wav_date_time <- stringr::str_extract(wav_path, '[0-9]{8}_[0-9]{6}')

  # create flac path
  flac_ssd_path <- stringr::str_glue('{hard_drive_path}/{stringr::str_extract(deployment_name, "G(P|R|C|M|0)[0-9]{2}_V[1-5]")}/{deployment_name}/{deployment_name}_{stringr::str_extract(wav_date_time, "[0-9]{8}")}/{deployment_name}_{wav_date_time}Z.flac')

  # wav to flac compression
  seewave::sox(
    stringr::str_glue('"{wav_path}" "{flac_ssd_path}"'),
    path2exe = "C:/Program Files (x86)/sox-14-4-2"
  )

}

# get name of an external hard drive (not just the drive letter)
get_volume_label <- function(drive_letter) {

  volumes <- system("wmic logicaldisk get name,volumename", intern = TRUE)
  lines <- volumes[-1]  # skip header
  lines <- trimws(lines)
  parts <- strsplit(lines, "\\s{2,}")  # split on 2+ spaces

  for (line in parts) {

    if (length(line) == 2 && startsWith(line[1], drive_letter)) {
      return(line[2])

    }

  }

  return(NA)

}
