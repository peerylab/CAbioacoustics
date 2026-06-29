
#' Play an audio file from R using SoX
#'
#' @param audio_path Path to audio file
#' @param begin_time When to start playing audio
#' @param end_time When to stop playing audio file
#'
#' @return Audio at particular start/end time
#' @export
#'
#' @examples
#' \dontrun{
#' # download an audio file from xeno-canto
#' tmp_file <- tempfile(fileext = ".wav")
#'
#' # store as a temporary file
#' download.file(
#'   "https://xeno-canto.org/877031/download",
#'   destfile = tmp_file,
#'   mode = ”wb”
#' )
#'
#' # play the audio with SoX (this function can also play wavs)
#' cb_play_audio(
#'   audio_path = tmp_file,
#'   begin_time = 0,
#'   end_time = 10
#' )
#' }

cb_play_audio <- function(audio_path, begin_time, end_time) {

  seewave::sox(
    command = stringr::str_glue("{audio_path} -t waveaudio trim {hms::as_hms(begin_time)} ={hms::as_hms(end_time)}"),
    path2exe = "C:/Program Files (x86)/sox-14-4-2"
  )

}
