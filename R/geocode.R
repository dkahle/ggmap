#' Geocode
#'
#' geocodes a location using Google Maps.
#' 
#' @param location a character string specifying a location of interest (e.g. "Baylor University")
#' @param output amount of output
#' @param messaging turn messaging on/off
#' @param sensor whether or not the geocoding request comes from a device with a location sensor
#' @param override_limit override the current query count (.GoogleGeocodeQueryCount)
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
#' geocode(c('baylor university', 'salvation army waco'))
#' geocode(c('baylor university', 'the vatican'))
#' geocode(c('baylor university', 'the vatican'), output = 'latlona')
#' geocode(c('baylor university', 'the vatican'), output = 'more')
#' geocode('the eiffel tower', output = 'all')
#' geocodeQueryCheck()
#'
#' # careful in running this...
#' library(stringr)
#' ads <- unique(crime$address)[1:120]
#' ads <- paste(ads, ', houston, texas', sep = '')
#' ads <- str_trim(ads)
#' gc <- geocode(ads)
#' }
#' 
geocode <- function (location, output = c('latlon','latlona','more','all'), 
  messaging = FALSE, sensor = FALSE, override_limit = FALSE)
{
	
  # check parameters
  stopifnot(is.character(location))
  output <- match.arg(output)
  stopifnot(is.logical(messaging))
	
  # vectorize for many locations (divide and conquer)
  if(length(location) > 1){ 
    s <- 'google restricts requests to 2500 requests a day.'
    if(length(location) > 2500) stop(s, call. = F)
    if(length(location) > 200 && messaging) message(paste('Reminder', s, sep = ' : '))      
    if(output == 'latlon' || output == 'latlona' ||output == 'more'){
      return(ldply(as.list(location), geocode, output = output, messaging = messaging))
    } else { # output = all
      return(llply(as.list(location), geocode, output = output, messaging = messaging))
    }
  }
    
  # format url
  sensor4url <- paste('sensor=', tolower(as.character(sensor)), sep = '')   
  loc <- location
  location <- gsub(' ', '+', location)
  posturl <- paste(location, sensor4url, sep = '&')        
  url_string <- paste('http://maps.googleapis.com/maps/api/geocode/json?address=', posturl, sep = "")
  url_string <- URLencode(url_string)
  if(messaging) message(paste('contacting ', url_string, '...', sep = ''), appendLF = F)
  
  # check/update google query limit
  check_geocode_query_limit(url_string, elems = 1, 
    override = override_limit, messaging = messaging)      
  
  # geocode
  connect <- url(url_string)
  gc <- fromJSON(paste(readLines(connect), collapse = ''))
  if(messaging) message(' done.')  
  close(connect)
  if(output == 'all') return(gc)  

  # did geocode fail?
  #print(gc$status)
  if(gc$status != 'OK'){
    warning(paste('geocode failed with status ', gc$status, ', location = "', 
      location, '"', sep = ''), call. = FALSE)
    return(data.frame(lon = NA, lat = NA))	
  }

  # more than one location found?
  if(length(gc$results) > 1 && messaging){
    message(paste(
      'more than one location found for "', loc, '", using address\n  "', 
      tolower(gc$results[[1]]$formatted_address), '"\n', sep = ''))
  }
    
  # format geocoded data
  NULLtoNA <- function(x){
    if(is.null(x)) return(NA)
    x
  }
  
  gcdf <- with(gc$results[[1]], {
  	data.frame(
      lon = NULLtoNA(geometry$location$lng),
      lat = NULLtoNA(geometry$location$lat),
      type = tolower(NULLtoNA(types[1])),      
      loctype = tolower(NULLtoNA(geometry$location_type)),
      address = tolower(NULLtoNA(formatted_address)),
      north = NULLtoNA(geometry$viewport$northeast$lat),
      south = NULLtoNA(geometry$viewport$southwest$lat),
      east = NULLtoNA(geometry$viewport$northeast$lng),
      west = NULLtoNA(geometry$viewport$southwest$lng)
    )
  })
  
  if(output == 'latlon') return(gcdf[,c('lon','lat')])
  if(output == 'latlona') return(gcdf[,c('lon','lat','address')])    
  
  # even more?
  attrdf <- ldply(gc$results[[1]]$address_components, function(l){
    as.data.frame(l, stringsAsFactors = FALSE)[1,]
  })
  attrdf <- attrdf[,c('types','long_name')]
  gcdf <- within(gcdf,{
    point_of_interest <- 
      tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'point_of_interest']))
    streetNo <- 
      as.numeric(NULLtoNA(attrdf$long_name[attrdf$types == 'street_number']))    
    street <- 
      tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'route']))
    locality <- 
      tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'locality']))    
    administrative_area_level_1 <- 
      tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'administrative_area_level_1']))                    
    administrative_area_level_2 <- 
      tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'administrative_area_level_2']))                
    country <- 
      tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'country']))      
    postal_code <- 
      tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'postal_code']))          
  })
  gcdf$query <- loc
  
  # return output = 'more'
  return(gcdf)
}













check_geocode_query_limit <- function(url_string, elems, override, messaging){  
  .GoogleGeocodeQueryCount <- NULL; rm(.GoogleGeocodeQueryCount); # R CMD check trick
  	
  if(exists('.GoogleGeocodeQueryCount', .GlobalEnv)){
    	
    .GoogleGeocodeQueryCount <<- 
      subset(.GoogleGeocodeQueryCount, time >= Sys.time() - 24*60*60)
    
    # 2500 per 24 hours
    if(sum(.GoogleGeocodeQueryCount$elements) + elems > 2500){
      message('query max exceeded, see ?geocode.  current total = ', 
        sum(.GoogleGeocodeQueryCount$elements))
      if(!override) stop('google query limit exceeded.', call. = FALSE)
    }
    
    # 10 per 1 second?
    if(with(.GoogleGeocodeQueryCount, 
      sum(elements[time >= Sys.time() - 10]) + elems > 10
    )){
      message('.', appendLF=F)
      Sys.sleep(1) # can do better
    }    
      
    # append to .GoogleGeocodeQueryCount
    .GoogleGeocodeQueryCount <<- rbind(.GoogleGeocodeQueryCount, 
      data.frame(time = Sys.time(),  url = url_string, 
        elements = elems, stringsAsFactors = FALSE)
    )
    
    	
  } else {
    	
    .GoogleGeocodeQueryCount <<- 
      data.frame(time = Sys.time(),  url = url_string, 
        elements = elems, stringsAsFactors = FALSE)
      
  }
}



#' Check Google Geocoding API query limit
#'
#' Check Google Geocoding API query limit
#' 
#' @return a data frame
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \url{https://developers.google.com/maps/documentation/geocoding/}
#' @export
#' @examples
#' geocodeQueryCheck()
geocodeQueryCheck <- function(){
  .GoogleGeocodeQueryCount <- NULL; rm(.GoogleGeocodeQueryCount); # R CMD check trick	
  if(exists('.GoogleGeocodeQueryCount', .GlobalEnv)){    	
  	remaining <- 2500-sum(
  	  subset(.GoogleGeocodeQueryCount, time >= Sys.time() - 24*60*60)$elements
  	  )
    message(remaining, ' geocoding queries remaining.')
  } else {
  	remaining <- 2500
    message(remaining, ' geocoding queries remaining.')
  }	
  invisible(remaining)
}