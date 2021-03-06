#' Get Products
#'
#' Retrieve products and related data for a shop
#'
#' @export
#'
#' @references \url{https://help.shopify.com/en/api/reference/products/product#index}
#'
#' @param shopURL shop URL (e.g. 'https://superstore-1.myshopify.com')
#' @param APIKey API key
#' @param APIPassword API password
#' @param APIVersion API version (default = \code{NULL} -> use the latest version)
#' @param max_pages maximum pages of records to return (i.e. the maximum number of HTTP GET requests to make). (default
#'   = \code{Inf})
#' @param limit_per_page maximum number of records to return per page (i.e. per HTTP GET request). Should be in the
#'   range [1, 250]. (default = 250)
#' @param ids vector of product ids like c(123, 456) (default = \code{NULL})
#' @param since_id only return orders with id > since_id (default = 0)
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
#' @param published_status published status. Should be \code{NULL} or one of \{'published', 'unpublished', 'any'\}.
#'   (default = \code{NULL})
#' @param fields fields to return. Common fields include:
#'
#'   \{'id', 'title', 'body_html', 'vendor', 'product_type', created_at', 'variants', 'options', 'images'\}
#'
#'   Only top-level fields are supported (i.e. you can't subset nested fields. For example, you can select 'variants'
#'   but not variants' subfields like 'price' or 'sku') (default = \code{NULL})
#' @param verbose should progress messages be printed? (default = \code{FALSE})
#' @return list of data.tables. If \code{fields} is \code{NULL}, all of the following will be returned:
#'
#'   \itemize{ \item products \item variants \item options \item images \item image.variant_ids }

shopr_get_products <- function(shopURL, APIKey, APIPassword, APIVersion = NULL, max_pages = Inf, limit_per_page = 250L,
                               ids = NULL, since_id = 0L, title = NULL, vendor = NULL, handle = NULL,
                               product_type = NULL, collection_id = NULL, created_at_min = NULL, created_at_max = NULL,
                               updated_at_min = NULL, updated_at_max = NULL, published_at_min = NULL,
                               published_at_max = NULL, published_status = NULL, fields = NULL, verbose = FALSE){

  #--- Validate Inputs --------------------------------------

  # Validate and clean fields
  created_at_min_ <- shopr_clean_datetime(created_at_min)
  created_at_max_ <- shopr_clean_datetime(created_at_max)
  updated_at_min_ <- shopr_clean_datetime(updated_at_min)
  updated_at_max_ <- shopr_clean_datetime(updated_at_max)
  published_at_min_ = shopr_clean_datetime(published_at_min)
  published_at_max_ = shopr_clean_datetime(published_at_max)
  ids_ <- shopr_clean_ids(ids = ids, resource = "products")
  since_id_ <- since_id
  fields_ <- shopr_clean_fields(fields = fields, resource = "products")
  published_status_ <- shopr_clean_published_status(published_status)

  # Validate limit_per_page
  if(limit_per_page < 1L | limit_per_page > 250L) stop("'limit_per_page' should be in the range [1, 250]")

  # Check if user is requesting for more ids than max_pages * limit_per_page
  if(!is.null(ids_) && length(stringr::str_split(ids_, ",")) > max_pages * limit_per_page){
    stop("Attempting to request more ids than max_pages * limit_per_page")
  }

  #--- API Version --------------------------------------
  # If APIVersion is NULL, use the latest

  APIVersion_ <- if(is.null(APIVersion)) shopr_get_latest_api_version() else APIVersion

  #--- Pagination --------------------------------------
  # Get the number of total products for the query parameters

  productsN <- shopr_get_products_count(
    shopURL = shopURL,
    APIKey = APIKey,
    APIPassword = APIPassword,
    APIVersion = APIVersion_,
    ids = ids_,
    since_id = since_id_,
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

  # Number of pages required to get all the data
  pagesRequired <- ceiling(productsN/limit_per_page)

  # Determine how many pages to query
  pagesN <- max(1L, min(pagesRequired, max_pages))

  #--- Request --------------------------------------

  # Build queryParams
  queryParams <- list(
    ids = if(is.null(ids_)) NULL else paste(ids_, collapse = ","),
    limit = limit_per_page,
    since_id = since_id_,
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
    published_status = published_status_,
    fields = fields_
  )

  # Build request
  requestURL <- paste0(shopURL, "/admin/api/", APIVersion_, "/products.json")
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
  products <- lapply(responses, function(x){
    data.table::as.data.table(jsonlite::fromJSON(
      txt = httr::content(x, "text", encoding = "UTF-8", flatten = TRUE)
    )$products)
  })

  # Collapse list of data.tables into a single data.table
  products <- data.table::rbindlist(products, use.names = TRUE, fill = TRUE)

  # If products is a NULL data.table, exit this function early
  isNULLDT <- all.equal(products, data.table::data.table())
  if(is.logical(isNULLDT) && isNULLDT == TRUE) return(products)

  ### Extract sub data.frames
  extract <- c("variants", "options", "images")
  result <- vector(mode = "list", length = 1 + length(extract))
  names(result) <- c("products", extract)
  for(df in extract){
    result[[df]] <- data.table::rbindlist(
      l = lapply(as.list(products[[df]]), data.table::as.data.table),
      use.names = TRUE,
      fill = TRUE
    )
  }

  # Insert products into result
  data.table::set(products, j = extract, value = NULL)
  result[["products"]] <- products

  # Return the result
  return(result[])
}
