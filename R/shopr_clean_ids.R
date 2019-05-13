shopr_clean_ids <- function(ids, resource, since_id = NULL){
  # Validate and clean ids
  # resource can be one of {'orders', 'products', 'inventory'}

  # Check if ids is NULL or length 0
  if(is.null(ids) || length(ids) == 0L){
    if(resource %in% c("inventory")) stop("'ids' cannot be NULL or length-0") else return(NULL)
  }

  # Make sure ids is a vector
  if(!is.vector(ids)) stop("'ids' should be a vector")
  if(is.character(ids) && any(stringr::str_detect(ids, "[^\\d]"))){
    stop("'ids' contains at least one non integer character")
  }

  # Check for dupes
  if(any(duplicated(ids))) stop("'ids' contains at least one duplicate")

  # Convert ids to character
  ids <- as.character(ids)

  # Remove ids <= since_id
  if(!is.null(since_id)){
    since_id <- as.character(since_id)
    ids <- ids[ids > since_id]
    if(length(ids) == 0) stop('No ids left after remove those <= since_id')
  }

  return(ids)
}
