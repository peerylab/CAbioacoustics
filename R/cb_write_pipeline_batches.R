
#' Write effort pipeline csv files
#'
#' @param df
#' @param output_path
#'
#' @return csv files of effort (i.e., row for each flac file)
#' @export
#'
#' @examples

cb_write_pipeline_batches <- function(df, output_path) {

  region <- unique(df$region)
  study <- unique(df$study_type)
  csv_num <- unique(df$csv_num)

  df |>
    dplyr::select(file_name) |>
    write.table(
      output_path,
      row.names = FALSE,
      col.names = FALSE
    )

}
