#' Geocode a dataset
#'
#' \code{mutate_geocode} geocodes a data frame and appends the new information
#' to the data frame provided.
#'
#' @param data a data frame
#' @param location a character string specifying a location of interest (e.g.
#'   "Baylor University")
#' @param ... arguments to pass to \code{\link{geocode}}
#' @return data with geocoded information appended as columns
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{geocode}}
#' @export
#' @examples
#'
#'
#' df <- data.frame(
#'   address = c("1600 Pennsylvania Avenue, Washington DC", "", "houston texas"),
#'   stringsAsFactors = FALSE
#' )
#'
#' \dontrun{ # Server response can be slow; this cuts down check time.
#' mutate_geocode(df, address)
#'
#'
#' library(dplyr)
#' df %>% mutate_geocode(address)
#' }
#'
mutate_geocode <- function(data, location, ...){
  locs <- data[, deparse(substitute(location)), drop = TRUE]
  gcdf <- geocode(locs, ...)
  data.frame(data, gcdf)
}










