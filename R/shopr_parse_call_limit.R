shopr_parse_call_limit <- function(xShopifyAPICallLimit){
  result <- list(
    bucketCalls = as.integer(stringr::str_extract(string = xShopifyAPICallLimit, pattern = "(?<=\\/)\\d+$")),
    bucketSize = as.integer(stringr::str_extract(string = xShopifyAPICallLimit, pattern = "^\\d+(?=\\/)"))
  )
  return(result)
}
