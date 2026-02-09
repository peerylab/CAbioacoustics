
#' Title
#'
#' @return
#' @export
#'
#' @examples

cb_create_acoustic_project <- function() {

  directories <- c(
    here::here('code'),
    here::here('log_files'),
    here::here('outputs'),
    here::here('files_to_delete')
  )

  directories |>
    fs::dir_create()

}
