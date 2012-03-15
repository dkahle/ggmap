#' Geocode
#'
#' geocodes a location using Google Maps.
#' 
#' @param location a character string specifying a location of interest (e.g. "Baylor University")
#' @param output amount of output
#' @param messaging turn messaging on/off
#' @return depends (at least a data.frame with variables lon and lat)
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @details note that the google maps api limits to 2500 queries a day.
#' @seealso \url{http://code.google.com/apis/maps/documentation/geocoding/}
#' @export
#' @examples
#'
#' 
#' \dontrun{
#' geocode('Baylor University')
#' geocode('1600 Pennsylvania Avenue, Washington DC')
#' geocode('1600 Pennsylvania Avenue, Washington DC', messaging = TRUE)
#' geocode('the white house', messaging = TRUE)
#' geocode('the eiffel tower')
#' geocode(c('baylor university', 'salvation army waco', 'HEB #087 waco'))
#' geocode(c('baylor university', 'the vatican'))
#' geocode(c('baylor university', 'the vatican'), output = 'latlona')
#' geocode(c('baylor university', 'the vatican'), output = 'more')
#' geocode('the eiffel tower', output = 'all')
#' }
#' 
geocode <- function (location, output = c('latlon','latlona','more','all'), messaging = FALSE){
  require(rjson)
	
  # check parameters
  stopifnot(is.character(location))
  output <- match.arg(output)
  stopifnot(is.logical(messaging))
	
  # vectorize for many locations (divide and conquer)
  if(length(location) > 1){ 
  	require(plyr)
    s <- 'google restricts requests to 2500 requests a day.'
    if(length(location) > 2500) stop(s, call. = F)
    if(length(location) > 200 && messaging) message(paste('Reminder', s, sep = ' : '))      
    if(output == 'latlon' || output == 'latlona' ||output == 'more'){
      return(ldply(as.list(location), geocode, output = output, messaging = messaging))
    } else { # output = all
      return(llply(as.list(location), geocode, output = output, messaging = messaging))
    }
  }
    
  # geocode
  url_string <- paste("http://maps.google.com/maps/geo?q=", location, sep = "")
  url_string <- URLencode(url_string)
  gc <- fromJSON(paste(readLines(url(url_string)), collapse = ''))
  closeAllConnections()
  if(output == 'all') return(gc)  

  # did geocode fail?
  if(gc$Status$code == 602){
    warning(paste('geocode failed - bad address? location = "', 
      location, '"', sep = ''), call. = FALSE)
    return(data.frame(lon = NA, lat = NA))	
  }
  if(gc$Status$code == 620){
    warning(paste('geocode failed - bad address? location = "', 
      location, '"', sep = ''), call. = FALSE)
    return(data.frame(lon = NA, lat = NA))	
  }  
    
  # more than one location found?
  if(length(gc$Placemark) > 1 && messaging){
    message(paste(
      'more than one location found for "', location, '", using address\n  "', 
      gc$Placemark[[1]]$address, '"\n', sep = ''))
  }
    
  # format geocoded data
  gcPm <- gc$Placemark[[1]]  
  with(gcPm, {gcdf <<- data.frame(
    lon = Point$coordinates[1],
    lat = Point$coordinates[2],
    z = Point$coordinates[3],  	
    address = tolower(address),
    north = ExtendedData$LatLonBox$north,
    south = ExtendedData$LatLonBox$south,
    east = ExtendedData$LatLonBox$east,
    west = ExtendedData$LatLonBox$west         
  )})
  if(output == 'latlon') return(gcdf[,c('lon','lat')])
  if(output == 'latlona') return(gcdf[,c('lon','lat','address')])  
  
  # add in 'more' details, country
  gcPmAdC <- gcPm$AddressDetails$Country  
  if(length(gcPmAdC$CountryName) > 0){
  	gcdf$country <- gcPmAdC$CountryName
  } else {
  	gcdf$country <- NA
  }  
  
  if(length(gcPmAdC$CountryNameCode) > 0){
  	gcdf$countryCode <- gcPmAdC$CountryNameCode
  } else {
  	gcdf$countryCode <- NA
  }    

  # add in 'more' details, (sub)administrative areas
  gcPmAdCAa <- gcPmAdC$AdministrativeArea
  if(length(gcPmAdCAa$AdministrativeAreaName) > 0){
  	gcdf$admin <- gcPmAdCAa$AdministrativeAreaName
  } else {
  	gcdf$admin <- NA
  }

  gcPmAdCAaSaa <- gcPmAdCAa$SubAdministrativeArea
  if(length(gcPmAdCAaSaa$Locality$LocalityName) > 0){
  	gcdf$locality <- gcPmAdCAaSaa$Locality$LocalityName
  } else {
  	gcdf$locality <- NA
  }  
  
  if(length(gcPmAdCAaSaa$SubAdministrativeAreaName) > 0){
  	gcdf$subadmin <- gcPmAdCAaSaa$SubAdministrativeAreaName
  } else {
  	gcdf$subadmin <- NA
  }      
  
  # return output = 'more'
  return(gcdf)
}