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
#' @param inventory_item_ids inventory item ids. At least one of \code{inventory_item_ids} and \code{location_ids} must
#'   be provided. (default = \code{NULL})
#' @param location_ids location ids. At least one of \code{inventory_item_ids} and \code{location_ids} must be provided.
#'   (default = \code{NULL})
#' @param updated_at_min POSIXct datetime or a string with format like '2014-04-25T16:15:47-04:00' (default =
#'   \code{NULL})
#' @param verbose should progress messages be printed? (default = \code{FALSE})
#' @return data.table of inventory levels

shopr_get_inventory_levels <- function(shopURL, APIKey, APIPassword, APIVersion = NULL, max_pages = Inf,
                                       limit_per_page = 250L, inventory_item_ids = NULL, location_ids = NULL,
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

  APIVersion_ <- if(is.null(APIVersion)) shopr_get_latest_api_version() else APIVersion

  #--- Pagination --------------------------------------

  if(!is.null(inventory_item_ids) && !is.null(location_ids)){
    chunksI <- ceiling(length(inventory_item_ids)/50)
    chunksL <- ceiling(length(location_ids)/50)
  } else if(!is.null(inventory_item_ids)){
    chunksI <- ceiling(length(inventory_item_ids)/50)
    chunksL <- 1L
  } else if(!is.null(location_ids)){
    chunksL <- ceiling(length(location_ids)/50)
    chunksI <- 1L
  }

  #--- Request --------------------------------------

  # Initialize responses, requestURL, num_pages
  responses <- vector(mode = "list", length = chunksL)
  for(k in seq_along(responses)) responses[[k]] <- vector(mode = "list", length = chunksI)
  requestURL <- paste0(shopURL, "/admin/api/", APIVersion_, "/inventory_levels.json")
  num_pages <- 0L

  # Loop over chunks of 50 locations
  for(l in seq_len(chunksL)){
    location_ids_l <- if(is.null(location_ids)) NULL else
      paste(location_ids[((l - 1) * 50 + 1):pmin(length(location_ids), l * 50)], collapse = ",")

    # Loop over chunks of 50 inventory items
    for(i in seq_len(chunksI)){
      inventory_item_ids_i <- if(is.null(inventory_item_ids)) NULL else
        paste(inventory_item_ids[((i - 1) * 50 + 1):pmin(length(inventory_item_ids), i * 50)], collapse = ",")

      # Print progress?
      if(verbose) print(paste0("Requesting chunk of pages: ", (l - 1) * chunksI + i, " of ", chunksL * chunksI))

      # Get the inventory levels for this chunk of (inv items, locations)
      queryParams <- list(
        limit = limit_per_page,
        location_ids = location_ids_l,
        inventory_item_ids = inventory_item_ids_i,
        updated_at_min = updated_at_min_
      )
      responses[[l]][[i]] <- shopr_make_requests(
        requestURL = requestURL,
        params = queryParams,
        pagesN = ceiling(50 * 50 / limit_per_page),
        maxPages = max_pages - num_pages,
        APIKey = APIKey,
        APIPassword = APIPassword,
        verbose = FALSE
      )

      # Update num_pages
      num_pages <- num_pages + length(responses[[l]][[i]])

      # Exit loop if max_pages has been reached
      if(num_pages == max_pages) break
    }

    # Exit loop if max_pages has been reached
    if(num_pages == max_pages) break
  }

  # Flatten the list of lists into just a list of responses
  responses <- do.call(c, unlist(responses, recursive = FALSE))

  # Check API version (but only if the user requested a specific version)
  if(isTRUE(responses[[1]]$headers$`x-shopify-api-version` != APIVersion)){
    warning(paste0(
      "Shopify processed this request with a different API version than the one you requested. ",
      "Requested: ", APIVersion, ", used: ", responses[[1]]$headers$`x-shopify-api-version`
    ))
  }

  #--- Parse responses --------------------------------------

  # Parse JSON
  invlevels <- lapply(responses, function(x){
    data.table::as.data.table(jsonlite::fromJSON(
      txt = httr::content(x, "text", encoding = "UTF-8", flatten = TRUE)
    )$inventory_levels)
  })

  # Collapse list of data.tables into a single data.table
  invlevels <- data.table::rbindlist(invlevels, use.names = TRUE, fill = TRUE)

  # Return the result
  return(invlevels[])
}
