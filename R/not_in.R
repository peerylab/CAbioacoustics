
#' Not in operator
#'
#' The `%ni%` operator is a convenience function that returns a logical vector
#' indicating whether elements of one vector are **not** present in another.
#' It is the inverse of the `%in%` operator.
#'
#' @param x A vector of values to be tested.
#' @param table A vector of values to be matched against.
#'
#' @return A logical vector the same length as `x`, with `TRUE` for elements
#' that are **not** found in `table` and `FALSE` for those that are.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' c("a", "b", "c") %ni% c("b", "d")
#' # Returns: TRUE FALSE TRUE
#'
#' # Equivalent to:
#' !(c("a", "b", "c") %in% c("b", "d"))
#' }
#' @export

"%ni%" <- function(x, table) {

  !(match(x, table, nomatch = 0) > 0)

}
