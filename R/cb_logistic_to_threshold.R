
#' Calculate BirdNET logit threshold(s) from logistic model of manually validated detections and their BirdNET logit score
#'
#' @param intercept Intercept value from logistic regression model
#' @param beta Slope value from logistic regression model
#' @param p_tp Probability of true positive detection value(s)
#'
#' @returns `Tibble` containing true positive detection probability(ies) and associated BirdNET logit threshold(s)
#' @export
#'
#' @examples
#' \dontrun{
#' cb_logistic_to_threshold(
#'   intercept = 0.830385299,
#'   beta = 1.111106172,
#'   p_tp = c(0.90, 0.95, 0.975, 0.99)
#'   )
#' }

cb_logistic_to_threshold <- function(intercept, beta, p_tp) {

  logit_threshold <- (log(p_tp / (1 - p_tp)) - intercept) / beta

  tibble::tibble(
    p_tp = p_tp,
    birdnet_logit_threshold = logit_threshold
  )

}
