
#' Save FLAC files from UW-Madison S3 locally. If needing to download a large batch of S3 FLACs, best to use `cb_globus_transfer`
#'
#' @param file Path to FLAC file on  UW-Madison S3
#' @param destination_folder Path to folder where FLAC will be downloaded
#'
#' @return FLAC file
#' @export
#'
#' @examples
#' \dontrun{
#' # path to FLAC on S3
#' amazon_s3_key <- '/Acoustic_Data/ARU_Data/ARU_Sierra_Monitoring/2023/North/G002_V1/G002_V1_C0142_U2/G002_V1_C0142_U2_20230509/G002_V1_C0142_U2_20230509_000100Z.flac'
#' # download it
#' cb_save_s3_flac(amazon_s3_key, here::here())
#' }

cb_save_s3_flac <- function(file, destination_folder) {

  aws.s3::save_object(
    object = file,
    bucket = "mpeery-archive",
    region = "",
    base_url = "s3.drive.wisc.edu",
    key = keyring::key_get('aws_access_key_id'),
    secret = keyring::key_get('aws_secret_access_key'),
    file = stringr::str_c(destination_folder, '/', basename(file))
  )

}
