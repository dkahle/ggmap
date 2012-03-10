#' Compute map distances using Google
#'
#' Compute map distances using Google Maps.
#' 
#' @param from name of origin addresses in a data frame (vector accepted)
#' @param to name of destination addresses in a data frame (vector accepted)
#' @param output amount of output
#' @param mode driving, bicycling, or walking
#' @param messaging turn messaging on/off
#' @param sensor whether or not the geocoding request comes from a device with a location sensor
#' @param language language
#' @return a data frame (output='simple') or all of the geocoded information (output='all')
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @details if parameters from and to are specified as geographic coordinates, they are reverse geocoded with revgeocode.  note that the google maps api limits to 2500 queries a day.
#' @export
#' @examples
#'
#' 
#' \dontrun{
#' 
#' from <- c('houston, texas', 'dallas')
#' to <- 'waco, texas'
#' mapdist(from, to)
#' mapdist(from, to, mode = 'bicycling')
#' mapdist(from, to, mode = 'walking')
#'
#' from <- c('houston', 'dallas')
#' to <- c('waco, texas', 'houston')
#' mapdist(from, to)
#'
#' mapdist('the white house', 'washington monument', mode = 'walking')
#' 
#' # geographic coordinates are accepted as well
#' (wh <- as.numeric(geocode('the white house')))
#' (wm <- as.numeric(geocode('washington monument')))
#' mapdist(wh, wm, mode = 'walking', messaging = TRUE)
#' mapdist('the white house', wm, mode = 'walking', messaging = TRUE)
#' 
#' }
#' 
mapdist <- function(from, to, mode = c('driving','walking','bicycling'), 
  output = c('simple','addresses','all'), messaging = FALSE, sensor = TRUE, language = 'en-EN')
{
  require(rjson)
  require(plyr)
	
  # check parameters
  if(is.numeric(from) && length(from) == 2){
    if(messaging) message('reverse geocoding "from" argument... ', appendLF = FALSE)  	
    from <- revgeocode(from)
    if(messaging) message('done.')
  }
  stopifnot(is.character(from))
  if(is.numeric(to) && length(to) == 2){
    if(messaging) message('reverse geocoding "to" argument... ', appendLF = FALSE)  	
    to <- revgeocode(to)
    if(messaging) message('done.')
  }
  stopifnot(is.character(to))  
  from_to_df <- data.frame(from = from, to = to, stringsAsFactors = FALSE)
  origins <- from_to_df$from 
  destinations <- from_to_df$to # this ensures # from = # to
  mode <- match.arg(mode)    
  output <- match.arg(output)  
  stopifnot(is.logical(messaging))
  stopifnot(is.logical(sensor))  
    
  # look up
  origins <- gsub(',', '', origins)
  origins <- gsub(' ', '+', origins)
  origins <- paste('origins=', paste(origins, collapse = '|'), sep = '')
  destinations <- gsub(',', '', destinations)
  destinations <- gsub(' ', '+', destinations)  
  destinations <- paste('destinations=', paste(destinations, collapse = '|'), sep = '')

  mode4url <- paste('mode=', mode, sep = '') 
  
  lang4url <- paste('language=', language, sep = '')   
  
  if(sensor){ sensor <- 'true' } else { sensor <- 'false' }
  sensor4url <- paste('sensor=', sensor, sep = '') 
  
  posturl <- paste(origins, destinations, mode4url, sensor4url, sep = '&')  
  
  url_string <- paste("http://maps.googleapis.com/maps/api/distancematrix/json?", 
    posturl, sep = "")
  url_string <- URLencode(url_string)
  tree <- fromJSON(paste(readLines(url(url_string)), collapse = ''))
  closeAllConnections()
  if(output == 'all') return(tree)
  
  # format output
  count <- 1
  out <- ldply(tree$rows, function(l){
  	e <- l$elements[[count]] 
  	count <<- count + 1
  	data.frame(
  	  m = e$distance$value,
  	  km = e$distance$value/1000,
  	  miles = 0.0006214 * e$distance$value,
  	  seconds = e$duration$value,
  	  minutes = e$duration$value / 60,
  	  hours = e$duration$value / 3600	  
  	)
  })
  
  # 'simple' return
  return(out) 
  
  # 'addresses' return  
  out$from <- tolower(tree$origin_addresses)
  out$to <- tolower(tree$destination_addresses)  
  return(out)   
}