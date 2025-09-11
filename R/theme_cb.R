
#' CAbioacoustics plotting theme
#'
#' @return
#' @export
#'
#' @examples

theme_cb <- function() {

  ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

}
