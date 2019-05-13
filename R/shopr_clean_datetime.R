shopr_clean_datetime <- function(datetime){
  # Validate datetimes and convert them to yyyy-mm-ddTHH:MM:SS-00:00
  # format: 2014-04-25T16:15:47-04:00

  if(is.null(datetime)){
    # Early exit in case datetime is NULL

    return(NULL)

  } else if(length(datetime) != 1L){
    # Make sure datetime is length 1

    stop("datetime should be length 1")

  } else if(is(datetime, "character")){
    # If datetime is a character, make sure it's in the proper format

    datetimePattern <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}\\:\\d{2}\\:\\d{2}-\\d{2}\\:\\d{2}$"
    if(!stringr::str_detect(string = datetime, pattern = datetimePattern)){
      stop("Improper datetime format. Should be like 2014-04-25T16:15:47-04:00")
    }

    return(datetime)

  } else if(is(datetime, "POSIXct")){
    # If datetime is a datetime (class POSIXct), return its character equivalent format in UTC time

    # Make a copy of datetime with the timezone changed to UTC
    datetimeUTC <- datetime
    attr(datetimeUTC, "tzone") <- "UTC"

    # Convert datetimeUTC to a character in the correct format
    datetimeUTC2 <- format(datetimeUTC, "%Y-%m-%dT%H:%M:%S-00:00")

    return(datetimeUTC2)

  } else {
    stop("Improper datetime class. Should be character or POSIXct")
  }
}
