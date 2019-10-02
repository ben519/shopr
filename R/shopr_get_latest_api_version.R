#' Get Latest Stable API Version
#'
#' Retrieve the latest stable api version
#'
#' Shopify releases a new API version every 3 months at the beginning of the quarter. Version names are date based to
#' be meaningful and semantically unambiguous (for example, 2020-01). Stable versions are released at 5pm UTC.
#'
#' @export
#'
#' @references \url{https://help.shopify.com/en/api/versioning#the-api-version-release-schedule}
#' @return character indicating the latest stable API version

shopr_get_latest_api_version <- function(){

  currentYr <- as.integer(format(Sys.Date(), "%Y"))
  apiversions <- seq.Date(
    from = as.Date(paste0(currentYr - 1, "-10-01")),
    to = as.Date(paste0(currentYr, "-10-01")),
    by = "3 months"
  )
  apiversions <- as.POSIXct(paste0(apiversions, " 17:00:00"), tz = "America/Chicago")
  currentTime <- Sys.time()

  for(i in seq_along(apiversions)){
    if(currentTime < apiversions[i]) break
  }

  v <- format(apiversions[i-1], "%Y-%m")

  return(v)
}
