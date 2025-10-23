
#' CAbioacoustics plotting theme
#'
#' @return A \code{ggplot2} object with the \code{CAbioacoustics} plotting theme
#' @export
#'
#' @examples
#' \dontrun{
#' # mtcars plot
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(x = hp, y = mpg, color = cyl, shape = as.factor(cyl))) +
#'   geom_point(size = 3) +
#'   theme_cb()
#' }

theme_cb <- function() {

  ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

}
