
#' Establish credentials for the UW spotted owl database
#'
#' @return Saved passwords for spotted owl database
#' @export
#'
#' @examples
#' \dontrun{
#' # automatically set database credentials with csv
#' cb_set_db_credentials('read_only_credentials.csv')
#' }

cb_set_db_credentials <- function(credentials_path) {

  credentials_df <- readr::read_csv(credentials_path)

  keyring::key_set(credentials_df$host)
  keyring::key_set(credentials_df$user)
  keyring::key_set(credentials_df$password)
  keyring::key_set(credentials_df$db)

}
