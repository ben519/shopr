shopr_clean_published_status <- function(published_status){

  if(is.null(published_status)){
    return(NULL)
  } else{
    published_statuses <- c("any", "published", "unpublished")
    if(!published_status %in% published_statuses){
      stop(paste0(
        "published_status = '", published_status, "', is not one of the allowed values {",
        paste(published_statuses, collapse = ','), "}"
      ))
    } else{
      return(published_status)
    }
  }
}
