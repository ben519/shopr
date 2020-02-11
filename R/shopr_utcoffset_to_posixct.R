#' UTCOffset To POSIXct
#'
#' Convert a UTC offset string (e.g. '2014-04-25T16:15:47-04:00') to POSIXct
#'
#' @export
#'
#' @references \url{https://en.wikipedia.org/wiki/UTC_offset}
#'
#' @param x character vector of UTC offset strings like '2014-04-25T16:15:47-04:00'
#' @param tz timezone (default = 'UTC')
#' @return vector of POSIXct
#'
#' @examples
#' foo <- c('2019-01-01T13:15:00-04:00', '2019-01-01T13:15:00+04:00')
#' shopr_UTCOffset_to_POSIXct(x = foo)
#' shopr_UTCOffset_to_POSIXct(x = foo, tz = 'America/Chicago')

shopr_UTCOffset_to_POSIXct <- function(x, tz = "UTC"){

  # Make sure x is in the proper format
  patternDT <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}\\:\\d{2}\\:\\d{2}(\\-|\\+)\\d{2}\\:\\d{2}$"
  if(!all(stringr::str_detect(string = x, pattern = patternDT), na.rm = TRUE)){
    stop("At least one x is not in UTC offset format like 2014-04-25T16:15:47-04:00")
  }

  # Extract the offset sign, hours and minutes from x
  offsetSigns <- stringr::str_extract(x, "(\\+|\\-)(?=\\d{2}\\:\\d{2}$)")
  offsetHrs <- as.numeric(stringr::str_extract(x, "\\d{2}(?=\\:\\d{2}$)"))
  offsetMins <- as.numeric(stringr::str_extract(x, "\\d{2}$"))

  # Calculate offset seconds
  offsetSecs <- ifelse(offsetSigns == "-", 1, -1) * 60*60*offsetHrs + 60*offsetMins

  # Make POSIXct UTC
  result <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = 'UTC') + offsetSecs

  # Convert timezone
  attr(result, "tzone") <- tz

  return(result)
}
