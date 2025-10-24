
#' Print Globus endpoint IDs
#'
#' @return UUIDs for available Globus endpoints
#' @export
#'
#' @examples
#' \dontrun{
#' # show endpoints
#' cb_show_globus_endpoints()
#' }

cb_show_globus_endpoints <- function() {

  message(stringr::str_glue("Amazon S3 endpoint = {keyring::key_get('s3_collection_id')}"))
  message(stringr::str_glue("Research Drive endpoint = {keyring::key_get('research_drive_collection_id')}"))
  message(stringr::str_glue("Jay's laptop = {keyring::key_get('jmwiniarski_laptop_collection_id')}"))

}
