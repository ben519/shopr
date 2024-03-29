#' Get Orders
#'
#' Retrieve orders and related data for a shop
#'
#' An order is a customer's completed request to purchase one or more products from a shop. An order is created when a
#' customer completes the checkout process, during which time they provide an email address or phone number, billing
#' address and payment information.
#'
#' @export
#'
#' @references \url{https://help.shopify.com/en/api/reference/orders/order#index}
#'
#' @param shopURL shop URL (e.g. 'https://superstore-1.myshopify.com')
#' @param APIKey API key
#' @param APIPassword API password
#' @param APIVersion API version (default = \code{NULL} -> use the latest version)
#' @param max_pages maximum pages of records to return (i.e. the maximum number of HTTP GET requests to make). (default
#'   = \code{Inf})
#' @param limit_per_page maximum number of records to return per page (i.e. per HTTP GET request). Should be in the
#'   range [1, 250]. (default = 250)
#' @param ids vector of order ids like c(123, 456) (default = \code{NULL})
#' @param since_id only return orders with id > since_id (default = 0)
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
#' @param fields fields to return. Common fields include:
#'
#'   \{"id", "email", "closed_at", "created_at", "updated_at", "number","note", "token", "gateway", "test",
#'   "total_price", "subtotal_price","total_weight", "total_tax", "taxes_included", "currency", "financial_status",
#'   "confirmed", "total_discounts", "total_line_items_price", "cart_token", "buyer_accepts_marketing", "name",
#'   "referring_site", "landing_site", "cancelled_at", "cancel_reason", "total_price_usd", "checkout_token",
#'   "reference", "user_id", "location_id", "source_identifier", "source_url", "processed_at", "device_id",
#'   "phone", "customer_locale", "app_id", "browser_ip", "landing_site_ref", "order_number", "discount_applications",
#'   "discount_codes", "note_attributes", "payment_gateway_names", "processing_method", "checkout_id", "source_name",
#'   "fulfillment_status", "tax_lines", "tags", "contact_email", "order_status_url", "presentment_currency",
#'   "total_line_items_price_set", "total_discounts_set", "total_shipping_price_set", "subtotal_price_set",
#'   "total_price_set", "total_tax_set", "total_tip_received", "admin_graphql_api_id", "line_items", "shipping_lines",
#'   "billing_address", "shipping_address", "fulfillments", "client_details", "refunds", "payment_details",
#'   "customer"\}
#'
#'   Only top-level fields are supported. (I.e. you can't subset nested fields. For example, you can select 'line_items'
#'   but not line_items' subfields like 'variant_id' or 'title'.) (default = \code{NULL})
#' @param verbose should progress messages be printed? (default = \code{FALSE})
#' @return list of data.tables. If \code{fields} is \code{NULL}, all of the following will be returned:
#'
#'   \itemize{ \item orders \item discount_applications \item discount_codes \item note_attributes \item tax_lines \item
#'   line_items \item shipping_lines \item fulfillments \item refunds }

shopr_get_orders <- function(shopURL, APIKey, APIPassword, APIVersion = NULL, max_pages = Inf, limit_per_page = 250L,
                             ids = NULL, since_id = 0L, created_at_min = NULL, created_at_max = NULL,
                             updated_at_min = NULL, updated_at_max = NULL, processed_at_min = NULL,
                             processed_at_max = NULL, status = 'any', financial_status = NULL,
                             fulfillment_status = NULL, fields = NULL, verbose = FALSE){

  #--- Validate Inputs --------------------------------------

  # Validate and clean fields
  created_at_min_ <- shopr_clean_datetime(created_at_min)
  created_at_max_ <- shopr_clean_datetime(created_at_max)
  updated_at_min_ <- shopr_clean_datetime(updated_at_min)
  updated_at_max_ <- shopr_clean_datetime(updated_at_max)
  processed_at_min_ = shopr_clean_datetime(processed_at_min)
  processed_at_max_ = shopr_clean_datetime(processed_at_max)
  ids_ <- shopr_clean_ids(ids = ids, resource = "orders")
  since_id_ <- since_id
  fields_ <- shopr_clean_fields(fields = fields, resource = "orders")
  status_ <- shopr_clean_order_status(status)
  financial_status_ <- shopr_clean_financial_status(financial_status)
  fulfillment_status_ <- shopr_clean_fulfillment_status(fulfillment_status)

  # Validate limit_per_page
  if(limit_per_page < 1L | limit_per_page > 250L) stop("'limit_per_page' should be in the range [1, 250]")

  # Clean some inputs
  if(length(ids_) > max_pages * limit_per_page){
    warning("Requesting more ids than max_pages * limit_per_page. Some ids may not be included in the result")
  }

  #--- API Version --------------------------------------
  # If APIVersion is NULL, use the latest

  APIVersion_ <- if(is.null(APIVersion)) shopr_get_latest_api_version() else APIVersion

  #--- Pagination --------------------------------------
  # Get the number of total orders for the query parameters

  ordersN <- shopr_get_orders_count(
    shopURL = shopURL,
    APIKey = APIKey,
    APIPassword = APIPassword,
    APIVersion = APIVersion_,
    ids = ids_,
    since_id = since_id_,
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

  # Number of pages required to get all the data
  pagesRequired <- ceiling(ordersN/limit_per_page)

  # Determine how many pages to query
  pagesN <- max(1L, min(pagesRequired, max_pages))

  #--- Request --------------------------------------

  # Build queryParams
  queryParams <- list(
    ids = if(is.null(ids_)) NULL else paste(ids_, collapse = ","),
    limit = limit_per_page,
    since_id = since_id_,
    created_at_min = created_at_min_,
    created_at_max = created_at_max_,
    updated_at_min = updated_at_min_,
    updated_at_max = updated_at_max_,
    processed_at_min = processed_at_min_,
    processed_at_max = processed_at_max_,
    status = status_,
    financial_status = financial_status_,
    fulfillment_status = fulfillment_status_,
    fields = fields_
  )

  # Build request
  requestURL <- paste0(shopURL, "/admin/api/", APIVersion_, "/orders.json")
  pg <- 1L

  # Make requests
  responses <- shopr_make_requests(
    requestURL = requestURL,
    params = queryParams,
    pagesN = pagesN,
    maxPages = max_pages,
    APIKey = APIKey,
    APIPassword = APIPassword,
    verbose = verbose
  )

  # Check API version (but only if the user requested a specific version)
  if(isTRUE(responses[[1L]]$headers$`x-shopify-api-version` != APIVersion)){
    warning(paste0(
      "Shopify processed this request with a different API version than the one you requested. ",
      "Requested: ", APIVersion, ", used: ", responses[[1L]]$headers$`x-shopify-api-version`
    ))
  }

  #--- Parse responses --------------------------------------

  # Parse JSON
  orders <- lapply(responses, function(x){
    data.table::as.data.table(jsonlite::fromJSON(
      txt = httr::content(x, "text", encoding = "UTF-8", flatten = FALSE)
    )$orders)
  })

  # Collapse list of data.tables into a single data.table
  orders <- data.table::rbindlist(orders, use.names = TRUE, fill = TRUE)

  # If orders is a NULL data.table, exit this function early
  isNULLDT <- all.equal(orders, data.table::data.table())
  if(is.logical(isNULLDT) && isNULLDT == TRUE) return(orders)

  # These fields cause problems.. remove them
  if("fulfillments" %in% colnames(orders)){
    for(i in seq_along(orders$fulfillments)){
      if("receipt" %in% colnames(orders$fulfillments[[i]])){
        data.table::set(orders$fulfillments[[i]], j = "receipt", value = NULL)
      }
      if("origin_address" %in% colnames(orders$fulfillments[[i]])){
        data.table::set(orders$fulfillments[[i]], j = "origin_address", value = NULL)
      }
    }
  }

  ### Extract sub data.frames
  subdataframes <- c("discount_applications", "discount_codes", "tax_lines", "line_items", "fulfillments", "refunds", "shipping_lines")
  extract <- intersect(colnames(orders), subdataframes)
  result <- vector(mode = "list", length = 1 + length(extract))
  names(result) <- c("orders", extract)
  for(df in extract){
    result[[df]] <- data.table::rbindlist(
      l = lapply(as.list(orders[[df]]), data.table::as.data.table),
      use.names = TRUE,
      fill = TRUE,
      idcol = TRUE
    )
    result[[df]]$order_id <- orders$id[result[[df]]$.id]
  }

  # Insert orders into result
  if(length(extract) > 0) data.table::set(orders, j = extract, value = NULL)
  result[["orders"]] <- orders

  # Return the result
  return(result[])
}
