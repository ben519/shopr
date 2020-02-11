#' Make Requests
#'
#' Generic helper method for making API requests that return paginated results using cursor-based pagination
#'
#' @export
#'
#' @references \url{https://help.shopify.com/en/api/guides/paginated-rest-results}
#'
#' @param requestURL e.g. https://abc.myshopify.com/admin/api/2020-01/orders.json?limit=250&since_id=0
#' @param params list of request parameters that must include 'limit'
#' @param pagesN expected number of pages to be returned (used for printing progress and initializing response list)
#' @param maxPages maximum number of pages to return (default = \code{Inf})
#' @param APIKey API key
#' @param APIPassword API password
#' @param verbose should progress messages be printed? (default = \code{FALSE})
#' @return list of http responses

shopr_make_requests <- function(requestURL, params, pagesN, maxPages = Inf, APIKey, APIPassword, verbose = FALSE){
  # Helper function for making API GET requests, particularly for dealing with pagination
  # See https://help.shopify.com/en/api/guides/paginated-rest-results
  # requestURL - initial request URL e.g. 'https://abc.myshopify.com/admin/api/2020-01/orders.json?limit=250'
  # params - list of parameters including "limit"
  # pagesN - expected number of pages to be returned
  # maxPages - max number of pages of data you want to fetch
  # Returns a list of responses

  # Check input
  if(pagesN < 1) stop("pagesN should be at least 1")
  if(maxPages < 1) stop("maxPages should be at least 1")
  if(!all(c("limit") %in% names(params))) stop("params should include 'limit'")

  # Initialize list to store responses, pg counter
  responses <- vector(mode = "list", length = pagesN)
  pg <- 1L

  # Make requests and generate responses
  while(TRUE){
    if(verbose) print(paste0("Requesting page: ", pg, " of ~", pagesN))
    if(length(responses) < pg) length(responses) <- 2*length(responses)

    # Occasionally httr::RETRY() throws a seemingly random error, "no applicable method for 'http_error'.."
    # If this happens, we catch the error and try fetching the data again, up to three times
    tryNumber = 1L
    while(is.null(responses[[pg]])){
      responses[[pg]] <- tryCatch(expr = {
        httr::RETRY(
          verb = "GET",
          url = requestURL,
          encode = "json",
          httr::authenticate(user = APIKey, password = APIPassword),
          query = if(pg == 1) params else params[c("limit", "fields")],
          quiet = !verbose
        )
      }, error = function(cond){
        if(stringr::str_detect(cond$message, "no applicable method for 'http_error'") & tryNumber < 3L){
          tryNumber <- tryNumber + 1L
          return(NULL)
        }  else{
          stop(cond)
        }
      })
    }

    # Get the next request URL if it exists, otherwise break
    if(!"link" %in% names(responses[[pg]]$headers)) break
    requestURL <- stringr::str_extract(responses[[pg]]$headers$link,"https(?:(?!https).)+(?=>; rel=\"next)")
    if(is.na(requestURL)) break

    # Check the current API call limit status. If the leaky call bucket is full, sleep for a bit
    # This logic is placed here because some fns like get_inventory_levels make repeated calls to this function
    callLimit <- shopr_parse_call_limit(responses[[pg]]$headers$`x-shopify-shop-api-call-limit`)
    if(callLimit$bucketCalls == callLimit$bucketSize) Sys.sleep(0.5)

    # If maxPages has been reached, break, otherwise increment pg
    if(pg == maxPages) break else pg <- pg + 1
  }

  # Filter our possible NULL values from responses
  responses <- Filter(f = Negate(is.null), x = responses)

  return(responses)
}
