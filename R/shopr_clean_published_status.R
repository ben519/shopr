#' Clean published status
#'
#' Helper function to validate and clean published status parameter
#'
#' @param published_status one of {'any', 'published', 'unpublished'}
#' @return string of clean published_status

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
