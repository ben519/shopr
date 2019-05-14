#' Get Inventory Levels
#'
#' Retrieve inventory levels for a shop
#'
#' An inventory level represents the available quantity of an inventory item at a specific location.
#'
#' Each inventory level belongs to one inventory item and has one location. For every location where an inventory item
#' is available, there's an inventory level that represents the inventory item's quantity at that location:
#'
#' @export
#'
#' @references \url{https://help.shopify.com/en/api/reference/inventory/inventorylevel#index}
#'
#' @param shopURL shop URL (e.g. 'https://superstore-1.myshopify.com')
#' @param APIKey API key
#' @param APIPassword API password
#' @param APIVersion API version (default = \code{NULL} -> use the latest version)
#' @param max_pages max number of pages of orders to return. (default = \code{Inf})
#' @param limit_per_page max number of results to return per request. Should be in the range [1, 250]. (default = 250)
#' @param page initial page of results to return. (default = 1)
#' @param inventory_item_ids inventory item ids. At least one of \code{inventory_item_ids} and \code{location_ids} must
#'   be provided. (default = \code{NULL})
#' @param location_ids location ids. At least one of \code{inventory_item_ids} and \code{location_ids} must be provided.
#'   (default = \code{NULL})
#' @param updated_at_min POSIXct datetime or a string with format like '2014-04-25T16:15:47-04:00' (default =
#'   \code{NULL})
#' @return data.table of inventory levels

shopr_get_inventory_levels <- function(shopURL, APIKey, APIPassword, APIVersion = NULL, max_pages = Inf,
                                       limit_per_page = 250L, page = 1L, inventory_item_ids = NULL, location_ids = NULL,
                                       updated_at_min = NULL, verbose = FALSE){

  #--- Validate Inputs --------------------------------------

  # Validate datetimes and convert them to yyyy-mm-ddTHH:MM:SS-00:00
  updated_at_min_ <- shopr_clean_datetime(updated_at_min)

  # Validate limit_per_page
  if(limit_per_page < 1L | limit_per_page > 250L) stop("'limit_per_page' should be in the range [1, 250]")

  # Validate inventory_item_ids and location_ids
  if(is.null(inventory_item_ids) && is.null(location_ids)){
    stop("Must provide 'inventory_item_ids' or 'location_ids' or both")
  }

  #--- API Version --------------------------------------
  # If APIVersion is NULL, use the latest
  # TODO: make this more dynamic

  APIVersion_ <- if(is.null(APIVersion)) "2019-04" else APIVersion

  #--- Pagination --------------------------------------

  pagesN <- max(1L, min(999999, max_pages))

  #--- Request --------------------------------------

  # List to store requests & results
  resultList <- vector(mode = "list", length = pagesN)

  # Build request
  requestURL <- paste0(shopURL, "/admin/api/", APIVersion_, "/inventory_levels.json")
  queryParams <- list(
    inventory_item_ids = if(is.null(inventory_item_ids)) NULL else paste(inventory_item_ids, collapse = ","),
    location_ids = if(is.null(location_ids)) NULL else paste(location_ids, collapse = ","),
    limit = limit_per_page,
    page = page
  )

  # Make requests and generate responses
  for(i in seq_len(pagesN)){
    if(verbose) print(paste0("Requesting page: ", i))
    response_i <- httr::RETRY(
      verb = "GET",
      url = requestURL,
      encode = "json",
      httr::authenticate(user = APIKey, password = APIPassword),
      query = queryParams,
      quiet = !verbose
    )
    resultList[[i]] <- jsonlite::fromJSON(
      txt = httr::content(response_i, "text", encoding = "UTF-8"),
      flatten = TRUE
    )$inventory_levels

    # Exit if no recrods found in the recent query
    if(length(resultList[[i]]) == 0) break

    # Update page
    queryParams$page <- queryParams$page + 1

    # Check the current API call limit status. If the leaky call bucket is full, sleep for a bit
    callLimit <- shopr_parse_call_limit(response_i$headers$`x-shopify-shop-api-call-limit`)
    if(callLimit$bucketCalls == callLimit$bucketSize) Sys.sleep(0.5)  # Sleep for 0.5 seconds
  }

  # Check API version (but only if the user requested a specific version)
  if(!is.null(APIVersion) && response_i$headers$`x-shopify-api-version` != APIVersion){
    warning(paste0(
      "Shopify processed this request with a different API version than the one you requested. ",
      "Requested: ", APIVersion, ", used: ", response_i$headers$`x-shopify-api-version`
    ))
  }

  #--- Clean up --------------------------------------

  # Collapse list of data.frames into a single data.table
  result <- data.table::rbindlist(resultList, use.names = TRUE, fill = TRUE)

  # Return the result
  return(result[])
}
