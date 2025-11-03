
#' Write effort pipeline csv files
#'
#' @param df
#'
#' @return csv files of effort (i.e., row for each flac file)
#' @export
#'
#' @examples

cb_write_pipeline_batches <- function(df) {

  region <- unique(df$region)
  study <- unique(df$study_type)
  csv_num <- unique(df$csv_num)

  df |>
    dplyr::select(file_name) |>
    write.table(
      stringr::str_glue(here::here('effort_pipelines/{region}_{study}_{csv_num}_pipeline.csv')),
      row.names = FALSE,
      col.names = FALSE
    )

}
