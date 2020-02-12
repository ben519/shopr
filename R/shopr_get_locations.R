#' Get Locations
#'
#' Retrieve the locations associated with a shop
#'
#' A location represents a geographical location where stores, pop-up stores, headquarters, and warehouses exist.
#' The Location resource can be used to track sales, manage inventory, and configure the tax rates to apply at checkout.
#'
#' @export
#'
#' @references \url{https://help.shopify.com/en/api/reference/inventory/location#index}
#'
#' @param shopURL shop url (e.g. 'https://superstore-1.myshopify.com')
#' @param APIKey API key
#' @param APIPassword API password
#' @param APIVersion API version
#' @param verbose should progress messages be printed? (default FALSE)
#' @return data.table of locations
#'
#' @examples
#' shopr_get_locations(
#'   shopURL = "https://superstore-1.myshopify.com",
#'   APIKey = "abc123",
#'   APIPassword = "123abc"
#' )

shopr_get_locations <- function(shopURL, APIKey, APIPassword, APIVersion = NULL, verbose = FALSE){

  #--- API Version --------------------------------------
  # If APIVersion is NULL, use the latest

  APIVersion_ <- if(is.null(APIVersion)) shopr_get_latest_api_version() else APIVersion

  #--- Request --------------------------------------

  # Build request
  requestURL <- paste0(shopURL, "/admin/api/", APIVersion_, "/locations.json")

  # Make request and generate response
  response <- httr::RETRY(
    verb = "GET",
    url = requestURL,
    encode = "json",
    httr::authenticate(user = APIKey, password = APIPassword),
    quiet = !verbose
  )

  # Check API version (but only if the user requested a specific version)
  if(!is.null(APIVersion) && response$headers$`x-shopify-api-version` != APIVersion){
    warning(paste0(
      "Shopify processed this request with a different API version than the one you requested. ",
      "Requested: ", APIVersion, ", used: ", response$headers$`x-shopify-api-version`
    ))
  }

  #--- Parse response --------------------------------------

  locations <- jsonlite::fromJSON(
    txt = httr::content(response, "text", encoding = "UTF-8"),
    flatten = TRUE
  )$locations

  # Convert to data.table
  data.table::as.data.table(locations)

  # Return the locations
  return(locations[])
}
