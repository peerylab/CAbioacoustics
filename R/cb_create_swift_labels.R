
#' Create PDF of scannable QR codes for Swift recorders (geared toward Avery 5520 labels)
#'
#' @param swift_df Data frame containing Swift IDs with `label` column
#' @param type QR or linear barcode
#' @param output_file PDF output path
#' @param font_size Size of Swift IDs on labels
#' @param label_height_ratio How high to make the Swift ID label
#'
#' @return PDF with QR codes/linear barcodes and Swift IDs
#' @export
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#'
#' # list of Swift IDs to create
#' swift_id <- 1:1000
#'
#' # add leading zeros and S
#' swift_df <-
#'   # always make swift ID be S and 4 digits
#'   sprintf("%004d", swift_id) |>
#'   as_tibble() |>
#'   rename(label = value) |>
#'   mutate(label = str_c('S', label))
#'
#' # create pdf of labels
#' cb_create_swift_labels(
#'   swift_df = swift_df,
#'   # matrix = QR code
#'   type = 'matrix',
#'   output_file = here::here('2025_sierra_monitoring_swift_labels.pdf'),
#'   font_size = 18,
#'   label_height_ratio = 1
#' )
#' }

cb_create_swift_labels <- function(swift_df, type = 'matrix', output_file, font_size, label_height_ratio) {

  num_row <- 10
  page_height <- 11
  height_margin = 0.5

  baRcodeR::custom_create_PDF(
    Labels = swift_df |> as.data.frame(),
    type = type,
    # .pdf not necessary
    name = stringr::str_remove(output_file, '.pdf'),
    Fsz = font_size,
    numrow = 10,
    numcol = 3,
    label_height = ((page_height - 2 * height_margin) / num_row) * label_height_ratio,
    width_margin = 3/8,
    height_margin = 1/2
  )

}
