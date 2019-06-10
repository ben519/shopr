#' Get Inventory Items
#'
#' Retrieve inventory items for a shop
#'
#' An inventory item represents the physical good available to be shipped to a customer. It holds essential information
#' about the physical good, including its SKU and whether its inventory is tracked.
#'
#' There is a 1:1 relationship between a product variant and an inventory item. Each product variant includes the ID of
#' its related inventory item. You can use the inventory item ID to query the InventoryLevel resource to retrieve
#' inventory information.
#'
#' Use the InventoryItem resource together with the InventoryLevel and Location resources to manage a store's inventory
#' across multiple locations.
#'
#' @export
#'
#' @references \url{https://help.shopify.com/en/api/reference/inventory/inventoryitem}
#'
#' @param shopURL shop URL (e.g. 'https://superstore-1.myshopify.com')
#' @param APIKey API key
#' @param APIPassword API password
#' @param APIVersion API version (default = \code{NULL} -> use the latest version)
#' @param max_pages maximum pages of records to return (i.e. the maximum number of HTTP GET requests to make). (default
#'   = \code{Inf})
#' @param limit_per_page maximum number of records to return per page (i.e. per HTTP GET request). Should be in the
#'   range [1, 250]. (default = 250)
#' @param ids vector of inventory item ids like c(123, 456) (REQUIRED)
#' @param since_id only return inventory items with id > since_id (default = 0)
#' @param verbose should progress messages be printed? (default = \code{FALSE})
#' @return data.table of inventory items

shopr_get_inventory_items <- function(shopURL, APIKey, APIPassword, APIVersion = NULL, max_pages = Inf,
                                      limit_per_page = 250L, ids, since_id = 0L, verbose = FALSE){

  #--- Validate Inputs --------------------------------------

  # Validate limit_per_page
  if(limit_per_page < 1L | limit_per_page > 250L) stop("'limit_per_page' should be in the range [1, 250]")

  # Clean some inputs
  ids_ <- shopr_clean_ids(ids = ids, resource = "inventory", since_id = since_id)
  ids_ <- as.numeric(ids_)
  ids_ <- sort(ids_)
  if(length(ids_) > max_pages * limit_per_page){
    warning("Requesting more ids than max_pages * limit_per_page. Some ids may not be included in the result")
  }
  since_id_ <- as.integer(since_id)

  #--- API Version --------------------------------------
  # If APIVersion is NULL, use the latest
  # TODO: make this more dynamic

  APIVersion_ <- if(is.null(APIVersion)) "2019-04" else APIVersion

  #--- Pagination --------------------------------------

  pagesN <- max(1L, min(max_pages, ceiling(length(ids_)/limit_per_page)))

  #--- Request --------------------------------------

  # List to store requests & results
  resultList <- vector(mode = "list", length = pagesN)

  # Build request
  requestURL <- paste0(shopURL, "/admin/api/", APIVersion_, "/inventory_items.json")
  queryParams <- list(
    ids = paste(head(ids_[ids_ > since_id_], limit_per_page), collapse = ","),
    limit = limit_per_page,
    since_id = since_id_
  )

  # Make requests and generate responses
  for(i in seq_len(pagesN)){
    if(verbose) print(paste0("Requesting page: ", i, " of ", pagesN))
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
    )$inventory_items

    # Update since_id, ids
    queryParams$since_id <- as.character(tail(resultList[[i]]$id, 1))
    queryParams$ids <- paste(head(ids_[ids_ > as.numeric(queryParams$since_id)], limit_per_page), collapse = ",")
    if(length(queryParams$ids) == 0) break

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
