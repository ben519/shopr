#' Get Orders Count
#'
#' Retrieve the number of orders for a shop satisfying some criteria
#'
#' @export
#'
#' @references \url{https://help.shopify.com/en/api/reference/orders/order#count}
#'
#' @param shopURL shop URL (e.g. 'https://superstore-1.myshopify.com')
#' @param APIKey API key
#' @param APIPassword API password
#' @param APIVersion API version (default = \code{NULL} -> use the latest version)
#' @param ids vector of order ids like c(123, 456) (default = \code{NULL})
#' @param since_id only consider orders with id > since_id (default = 0)
#' @param created_at_min POSIXct datetime or a string with format like '2014-04-25T16:15:47-04:00' (default =
#'   \code{NULL})
#' @param created_at_max POSIXct datetime or a string with format like '2014-04-25T16:15:47-04:00' (default =
#'   \code{NULL})
#' @param updated_at_min POSIXct datetime or a string with format like '2014-04-25T16:15:47-04:00' (default =
#'   \code{NULL})
#' @param updated_at_max POSIXct datetime or a string with format like '2014-04-25T16:15:47-04:00' (default =
#'   \code{NULL})
#' @param processed_at_min POSIXct datetime or a string with format like '2014-04-25T16:15:47-04:00' (default =
#'   \code{NULL})
#' @param processed_at_max POSIXct datetime or a string with format like '2014-04-25T16:15:47-04:00' (default =
#'   \code{NULL})
#' @param status order status. Should be \code{NULL} or one of \{'open', 'closed', 'any'\}. (default = \code{'any'})
#' @param financial_status financial status. Should be \code{NULL} or one of \{'any', 'authorized', 'pending', 'paid',
#'   'refunded', 'voided'\}. (default = \code{NULL})
#' @param fulfillment_status fulfillment status. Should be \code{NULL} or one of \{'any', 'shipped', 'partial',
#'   'unshipped'\}. (default = \code{NULL})
#' @param verbose should progress messages be printed? (default = \code{FALSE})
#' @return number of orders

shopr_get_orders_count <- function(shopURL, APIKey, APIPassword, APIVersion = NULL, ids = NULL, since_id = NULL,
                                   created_at_min = NULL, created_at_max = NULL, updated_at_min = NULL,
                                   updated_at_max = NULL, processed_at_min = NULL, processed_at_max = NULL,
                                   status = 'any', financial_status = NULL, fulfillment_status = NULL, verbose = FALSE){

  #--- Validate Inputs --------------------------------------

  # Validate and clean fields
  created_at_min_ <- shopr_clean_datetime(created_at_min)
  created_at_max_ <- shopr_clean_datetime(created_at_max)
  updated_at_min_ <- shopr_clean_datetime(updated_at_min)
  updated_at_max_ <- shopr_clean_datetime(updated_at_max)
  processed_at_min_ <- shopr_clean_datetime(processed_at_min)
  processed_at_max_ <- shopr_clean_datetime(processed_at_max)
  ids_ <- shopr_clean_ids(ids = ids, resource = "orders")
  status_ <- shopr_clean_order_status(status)
  financial_status_ <- shopr_clean_financial_status(financial_status)
  fulfillment_status_ <- shopr_clean_fulfillment_status(fulfillment_status)

  #--- API Version --------------------------------------
  # If APIVersion is NULL, use the latest
  # TODO: make this more dynamic

  APIVersion_ <- if(is.null(APIVersion)) "2019-04" else APIVersion

  #--- Request --------------------------------------

  requestURL <- paste0(shopURL, "/admin/api/", APIVersion_, "/orders/count.json")
  queryParams <- list(
    ids = if(is.null(ids_)) NULL else paste(ids_, collapse = ","),
    since_id = since_id,
    created_at_min = created_at_min_,
    created_at_max = created_at_max_,
    updated_at_min = updated_at_min_,
    updated_at_max = updated_at_max_,
    processed_at_min = processed_at_min_,
    processed_at_max = processed_at_max_,
    status = status_,
    financial_status = financial_status_,
    fulfillment_status = fulfillment_status_
  )

  # Make the request and get the response
  response <- httr::RETRY(
    verb = "GET",
    url = requestURL,
    encode = "json",
    httr::authenticate(user = APIKey, password = APIPassword),
    query = queryParams,
    quiet = !verbose
  )

  # Check API version (but only if the user requested a specific version)
  if(!is.null(APIVersion) && response$headers$`x-shopify-api-version` != APIVersion){
    warning(paste0(
      "Shopify processed this request with a different API version than the one you requested. ",
      "Requested: ", APIVersion, ", used: ", response$headers$`x-shopify-api-version`
    ))
  }

  #--- Clean up --------------------------------------

  # Parse the response
  result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))$count

  # Return the result
  return(result)
}
