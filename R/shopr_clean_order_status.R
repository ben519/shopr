#' Clean order status
#'
#' Helper function to validate and clean order status parameter
#'
#' @param order_status one of {'open', 'closed', 'any'}
#' @return string of clean order_status

shopr_clean_order_status <- function(order_status){

  if(is.null(order_status)){
    return(NULL)
  } else{
    order_statuses <- c("open", "closed", "any")
    if(!order_status %in% order_statuses){
      stop(paste0(
        "status = '", order_status, "', is not one of the allowed values {", paste(order_status, collapse = ','), "}"
      ))
    } else{
      return(order_status)
    }
  }
}
