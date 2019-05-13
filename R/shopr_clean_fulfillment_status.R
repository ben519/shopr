shopr_clean_fulfillment_status <- function(fulfillment_status){

  if(is.null(fulfillment_status)){
    return(NULL)
  } else{
    fulfillment_statuses <- c("any", "shipped", "partial", "unshipped")
    if(!fulfillment_status %in% fulfillment_statuses){
      stop(paste0(
        "fulfillment_status = '", fulfillment_status, "', is not one of the allowed values {",
        paste(fulfillment_status, collapse = ','), "}"
      ))
    } else{
      return(fulfillment_status)
    }
  }
}
