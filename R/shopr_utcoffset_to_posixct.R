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
#' shopr_UTCOffset_to_POSIXct(x = c('2019-01-01T13:15:00-04:00', '2019-01-01T13:15:00+04:00'))
#' shopr_UTCOffset_to_POSIXct(x = c('2019-01-01T13:15:00-04:00', '2019-01-01T13:15:00+04:00'), tz = 'America/Chicago')

shopr_UTCOffset_to_POSIXct <- function(x, tz = "UTC"){

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
