
#' Execute a file transfer between two Globus collections from R
#'
#' @param collection_one Globus collection (source collection UUID)
#' @param collection_two Globus collection (destination collection UUID)
#' @param destination_path Path to destination for `collection_two`
#' @param batch_file_path A .txt file (i.e., batch file) containing path to files in source collection (1st column) and destination paths (2nd column)
#' @param label Label for Globus transfer
#'
#' @return Audio files copied to `collection_two`
#' @export
#'
#' @examples
#' \dontrun{
#' # create name for transfer
#' transfer_label <- stringr::str_c('example_transfer', Sys.Date(), sep = '_')
#'
#' # find batch file
#' batch_file_path <- here::here('in.txt')
#'
#' # additional path info for destination two
#' destination_path <- 'data_requests/example_request/raw_flacs'
#'
#' # run the transfer
#' cb_globus_transfer(
#' # collection UUIDs saved in keyring already
#'   collection_one = 's3_collection_id',
#'   collection_two = 'research_drive_collection_id',
#'   destination_path = destination_path,
#'   batch_file_path = batch_file_path,
#'   label = transfer_label
#' )
#' }

cb_globus_transfer <- function(collection_one, collection_two, destination_path, batch_file_path, label) {

  globus_transfer <-
    stringr::str_glue(
      'globus transfer {keyring::key_get(collection_one)}:/ {keyring::key_get(collection_two)}:{destination_path} --label "{label}" --batch {batch_file_path}'
    )

  # print transfer
  print(globus_transfer)

  # run the transfer
  system(globus_transfer)

}
