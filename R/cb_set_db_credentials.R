
#' Establish credentials for the UW spotted owl database
#'
#' @param credentials_path Path to csv with database credentials
#'
#' @return Saved passwords in keyring for spotted owl database
#' @export
#'
#' @examples
#' \dontrun{
#' # automatically set database credentials with csv
#' cb_set_db_credentials('read_only_credentials.csv')
#' }

cb_set_db_credentials <- function(credentials_path) {

  credentials_df <- readr::read_csv(credentials_path)

  keyring::key_set_with_value(service = 'spotted_owl_host', password = credentials_df$host)
  keyring::key_set_with_value(service = 'spotted_owl_user', password = credentials_df$user)
  keyring::key_set_with_value(service = 'spotted_owl_pwd', password = credentials_df$password)
  keyring::key_set_with_value(service = 'spottedowl_db', password = credentials_df$db)

}
