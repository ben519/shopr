#' Get Products Count
#'
#' Retrieve the number of products for a shop satisfying some criteria
#'
#' @export
#'
#' @references \url{https://help.shopify.com/en/api/reference/products/product#count}
#'
#' @param shopURL shop URL (e.g. 'https://superstore-1.myshopify.com')
#' @param APIKey API key
#' @param APIPassword API password
#' @param APIVersion API version (default = \code{NULL} -> use the latest version)
#' @param ids vector of product ids like c(123, 456) (default = \code{NULL})
#' @param since_id only consider products with id > since_id (default = 0)
#' @param title product title (default = \code{NULL})
#' @param vendor product vendor (default = \code{NULL})
#' @param handle product handle (default = \code{NULL})
#' @param product_type product type (default = \code{NULL})
#' @param collection_id collection id (default = \code{NULL})
#' @param created_at_min POSIXct datetime or a string with format like '2014-04-25T16:15:47-04:00' (default =
#'   \code{NULL})
#' @param created_at_max POSIXct datetime or a string with format like '2014-04-25T16:15:47-04:00' (default =
#'   \code{NULL})
#' @param updated_at_min POSIXct datetime or a string with format like '2014-04-25T16:15:47-04:00' (default =
#'   \code{NULL})
#' @param updated_at_max POSIXct datetime or a string with format like '2014-04-25T16:15:47-04:00' (default =
#'   \code{NULL})
#' @param published_at_min POSIXct datetime or a string with format like '2014-04-25T16:15:47-04:00' (default =
#'   \code{NULL})
#' @param published_at_max POSIXct datetime or a string with format like '2014-04-25T16:15:47-04:00' (default =
#'   \code{NULL})
#' @param published_status published status. Should be NULL or one of \{'published', 'unpublished', 'any'\}. (default =
#'   \code{NULL})
#' @return number of products

shopr_get_products_count <- function(shopURL, APIKey, APIPassword, APIVersion = NULL, ids = NULL, since_id = NULL,
                                     title = NULL, vendor = NULL, handle = NULL, product_type = NULL,
                                     collection_id = NULL, created_at_min = NULL, created_at_max = NULL,
                                     updated_at_min = NULL, updated_at_max = NULL, published_at_min = NULL,
                                     published_at_max = NULL, published_status = NULL){
  # Some veriables go through a transformation. We use a trailing underscore to identify the transformed var
  # created_at_min can be a datetime (POSIXct) or a character in the format specified by shopify

  #--- Validate Inputs --------------------------------------

  # Validate and clean inputs
  created_at_min_ <- shopr_clean_datetime(created_at_min)
  created_at_max_ <- shopr_clean_datetime(created_at_max)
  updated_at_min_ <- shopr_clean_datetime(updated_at_min)
  updated_at_max_ <- shopr_clean_datetime(updated_at_max)
  published_at_min_ <- shopr_clean_datetime(published_at_min)
  published_at_max_ <- shopr_clean_datetime(published_at_max)
  ids_ <- shopr_clean_ids(ids = ids, resource = "products")
  published_status_ <- shopr_clean_published_status(published_status)

  #--- API Version --------------------------------------
  # If APIVersion is NULL, use the latest

  APIVersion_ <- if(is.null(APIVersion)) shopr_get_latest_api_version() else APIVersion

  #--- Request --------------------------------------

  requestURL <- paste0(shopURL, "/admin/api/", APIVersion_, "/products/count.json")
  queryParams <- list(
    ids = ids,
    since_id = since_id,
    title = title,
    vendor = vendor,
    handle = handle,
    product_type = product_type,
    collection_id = collection_id,
    created_at_min = created_at_min_,
    created_at_max = created_at_max_,
    updated_at_min = updated_at_min_,
    updated_at_max = updated_at_max_,
    published_at_min = published_at_min_,
    published_at_max = published_at_max_,
    published_status = published_status_
  )

  # Make the request and get the response
  response <- httr::RETRY(
    verb = "GET",
    url = requestURL,
    encode = "json",
    httr::authenticate(user = APIKey, password = APIPassword),
    query = queryParams,
    quiet = TRUE
  )

  # Check API version (but only if the user requested a specific version)
  if(isTRUE(response$headers$`x-shopify-api-version` != APIVersion)){
    warning(paste0(
      "Shopify processed this request with a different API version than the one you requested. ",
      "Requested: ", APIVersion, ", used: ", response$headers$`x-shopify-api-version`
    ))
  }

  #--- Clean up --------------------------------------

  # Parse the response
  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  if(!is.null(result$errors)) stop(as.character(result$errors))
  result <- result$count

  # Return the result
  return(result)
}
