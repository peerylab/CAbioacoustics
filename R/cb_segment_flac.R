
#' Segment a FLAC file using the begin/end time of a BirdNET selection
#'
#' @param input_flac_path Path for input FLAC file
#' @param output_flac_path Path to store FLAC segment
#' @param begin_time Time from start of recording (beginning of a selection)
#' @param end_time Time from start of recording (end of selection)
#'
#' @return FLAC file segment
#' @export
#'
#' @examples
#' \dontrun{
#' # set FLAC parameters
#' input_flac_path <- "Z:/Acoustic_Data/ARU_Data_Raw/ARU_Sierra_Monitoring/2025/South/G055_V1/G055_V1_C4928_U1/G055_V1_C4928_U1_20250519/G055_V1_C4928_U1_20250519_060002Z.flac"
#' output_flac_path <- "C:/Users/jmwin/OneDrive/Desktop/test_segment.flac"
#' begin_time <- 3
#' end_time <- 6
#'
#' # create FLAC segment
#' cb_segment_flac(
#'   input_flac_path,
#'   output_flac_path,
#'   begin_time,
#'   end_time
#' )
#' }

cb_segment_flac <- function(input_flac_path, output_flac_path, begin_time, end_time) {

  # trim flac
  seewave::sox(
    stringr::str_glue("{input_flac_path} {output_flac_path} trim {hms::as_hms(begin_time)} ={hms::as_hms(end_time)}"),
    path2exe = "C:/Program Files (x86)/sox-14-4-2"
  )

}
