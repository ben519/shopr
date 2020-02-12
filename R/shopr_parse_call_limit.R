#' Prase Call Limit
#'
#' Helper function to parse the api call limit given the latest response
#'
#' @param xShopifyAPICallLimit header field xShopifyAPICallLimit from recent response
#' @return list of {bucketCalls, bucketSize}

shopr_parse_call_limit <- function(xShopifyAPICallLimit){
  result <- list(
    bucketCalls = as.integer(stringr::str_extract(string = xShopifyAPICallLimit, pattern = "(?<=\\/)\\d+$")),
    bucketSize = as.integer(stringr::str_extract(string = xShopifyAPICallLimit, pattern = "^\\d+(?=\\/)"))
  )
  return(result)
}
