#' Geocode
#'
#' geocodes a location using google maps.
#' 
#' @param location a character string specifying a location of interest (e.g. "Baylor University")
#' @return a named numeric vector of geographical coordinates.
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @export
#' @examples
#'
#' 
#' \dontrun{
#' geocode('Baylor University')
#' geocode('1600 Pennsylvania Avenue, Washington DC')
#' geocode('the eiffel tower')
#' }
#' 
geocode <- function(location){
  location_splt <- strsplit(location, split = ' ')[[1]]
  url_string <- paste('http://maps.google.com/maps/geo?q=', 
    paste(location_splt, collapse = ',+'), sep = '')
  site   <- readLines(url(url_string))	
  site   <- site[which(regexpr('coordinates', site) > 0)]
  if(is.na(site[1])) stop('location geocoding error.')
  site   <- strsplit(site, '\\[')[[1]][2]
  site   <- strsplit(site, ',')[[1]][1:2]
  latlon <- as.numeric(site)	
  center <- c(lon = latlon[1], lat = latlon[2])
  closeAllConnections()
  center
}
