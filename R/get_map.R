#' Grab a map.
#'
#' get_map is a smart function which queries the Google Maps, OpenStreetMap, or Stamen Maps server for a map at a certain location at a certain spatial zoom.  it is a wrapper for get_googlemap, get_openstreetmap, get_stamenmap, and get_cloudmademap functions.  get_map was formerly (<2.0) called ggmap.
#' 
#' @param location an address, longitude/latitude pair (in that order), or left/bottom/right/top bounding box
#' @param zoom map zoom, an integer from 0 (whole world) to 21 (building), default value 10 (city).  openstreetmaps limits a zoom of 18, and the limit on stamen maps depends on the maptype
#' @param scale scale, see \code{\link{get_openstreetmap}}
#' @param maptype character string providing map theme. options available are 'terrain', 'satellite', 'roadmap', and 'hybrid' (google maps), 'terrain', 'watercolor', and 'toner' (stamen maps), or a positive integer for cloudmade maps (see ?get_cloudmademap)
#' @param source Google Maps ('google'), OpenStreetMap ('osm'), Stamen Maps ('stamen'), or CloudMade maps ('cloudmade')
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param filename destination file for download (file extension added according to format)
#' @param crop (stamen and cloudmade maps) crop tiles to bounding box
#' @param color color ('color') or black-and-white ('bw')
#' @param api_key an api key for cloudmade maps
#' @return a data.frame with columns latitude, longitude, and fill
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{ggmap}}, \code{\link{GetMap}} in package RgoogleMaps
#' @export
#' @examples
#'
#' 
#' \dontrun{ 
#' map <- get_map()
#' ggmap(map, fullpage = TRUE)
#' 
#' map <- get_map(maptype = 'roadmap')
#' ggmap(map, fullpage = TRUE)
#' 
#' map <- get_map(maptype = 'hybrid')
#' ggmap(map, fullpage = TRUE)
#' 
#' map <- get_map(maptype = 'satellite')
#' ggmap(map, fullpage = TRUE)
#' 
#' map <- get_map(source = 'osm')
#' ggmap(map, fullpage = TRUE)
#' 
#' map <- get_map(source = 'stamen')
#' ggmap(map, fullpage = TRUE)
#' 
#' map <- get_map(source = 'stamen', maptype = 'watercolor')
#' ggmap(map, fullpage = TRUE)
#' 
#' map <- get_map(source = 'stamen', maptype = 'toner')
#' ggmap(map, fullpage = TRUE)
#' 
#' 
#'
#' map <- get_map(location = 'texas', zoom = 6, source = 'stamen')
#' ggmap(map, fullpage = TRUE)
#' 
#' map <- get_map(location = 'united states', zoom = 4, source = 'stamen')
#' ggmap(map, fullpage = TRUE)
#' 
#' api_key <- '<your api key here>'
#' map <- get_map(location = 'baylor university', source = 'cloudmade', 
#'   maptype = 53428, api_key = api_key, zoom = 14)
#' ggmap(map, fullpage = TRUE)
#' 
#' 
#' }
#' 
get_map <- function(
  location = c(lon = -95.3632715, lat = 29.7632836), zoom = 10, scale = 'auto',
  maptype = c('terrain', 'satellite', 'roadmap', 'hybrid','toner','watercolor'), 
  messaging = FALSE, urlonly = FALSE, filename = 'ggmapTemp', crop = TRUE,
  color = c('color','bw'), source = c('google','osm','stamen','cloudmade'),
  api_key
){
	
  # deprecated syntaxes
  args <- as.list(match.call(expand.dots = TRUE)[-1])  
  if('verbose' %in% names(args)){
    warning('verbose argument deprecated, use messaging.', call. = F)
    messaging <- eval(args$verbose)
  }
  
  if('center' %in% names(args)){
    warning('center argument deprecated, use location.', call. = F)
    location <- eval(args$center)
  }
  
  
  # preliminary argument checking
  source <- match.arg(source)  
  color <- match.arg(color)  
  if(missing(maptype)){
    if(source != 'cloudmade'){
      maptype <- 'terrain'	
    } else {
      maptype <- 1
    }
  }  
  if(source == 'stamen'){
    if(!(maptype %in% c('terrain','watercolor','toner'))){
      stop('when using stamen maps, only terrain, watercolor, and toner maptypes available',
        call. = FALSE)
    }
  }
  if(scale == 'auto'){
  	if(source == 'google') scale <- 2
  	if(source == 'osm') scale <- OSM_scale_lookup(zoom)
  }

  
  
  # location formatting
  location_stop <- TRUE
  if(is.character(location) && length(location) == 1){
    location_type <- 'address'
    location_stop <- FALSE    
  }
  if(is.numeric(location) && length(location) == 2){
    location_type <- 'lonlat'
    location_stop <- FALSE      	
    if(!is.null(names(location))){
      loc_names <- names(location)
      if(all(loc_names == c('long','lat'))){
        names(location) <- c('lon', 'lat')
      } else if(all(loc_names == c('lat','lon'))){
      	message('note : locations should be specified in the lon/lat format, not lat/lon.')
      	location <- location[c('lon','lat')]      	
      } else if(all(loc_names == c('lat','long'))){
      	message('note : locations should be specified in the lon/lat format, not lat/lon.')
      	location <- location[c('long','lat')]    
        names(location) <- c('lon', 'lat')      	  	
      }
    } else { # is missing name the elements lon/lat
      names(location) <- c('lon','lat')
    }
  }
  if(is.numeric(location) && length(location) == 4){
    location_type <- 'bbox'
    location_stop <- FALSE      	
  }  
  if(location_stop){
    stop('improper location specification, see ?get_map.', call. = F)
  }



  # google map
  if(source == 'google'){
    return(
      get_googlemap(center = location, zoom = zoom, maptype = maptype, scale = scale,
        messaging = messaging, urlonly = urlonly, filename = filename, 
        color = color, checkargs = FALSE)
    )
  }


  # openstreetmap
  if(source == 'osm'){
  	
  	if(location_type != 'bbox'){
      gm <- get_googlemap(center = location, zoom = zoom, 
        filename = filename, checkargs = FALSE)
      location <- as.numeric(attr(gm, 'bb'))[c(2,1,4,3)]
    }  
  	
    return(
      get_openstreetmap(bbox = location, scale = scale, 
        messaging = messaging, urlonly = urlonly, filename = filename, 
        color = color, checkargs = FALSE)
    )
  }
  
  
  # stamen map
  if(source == 'stamen'){
  	if(location_type != 'bbox'){
      gm <- get_googlemap(center = location, zoom = zoom, 
        filename = filename, checkargs = FALSE)
      location <- as.numeric(attr(gm, 'bb'))[c(2,1,4,3)]
    }     
  	
    return(
      get_stamenmap(bbox = location, zoom = zoom, maptype = maptype, crop = crop,
        messaging = messaging, urlonly = urlonly, filename = filename, 
        color = color, checkargs = FALSE)
    )
  }  
  
  # cloudmade map
  if(source == 'cloudmade'){
  	if(missing(api_key)) stop('an api key must be provided for cloudmade maps, see ?get_cloudmademap.',
  	  call. = FALSE)
  	
  	if(location_type != 'bbox'){
      gm <- get_googlemap(center = location, zoom = zoom, 
        filename = filename, checkargs = FALSE)
      location <- as.numeric(attr(gm, 'bb'))[c(2,1,4,3)]
    }     
  	
    return(
      get_cloudmademap(bbox = location, zoom = zoom, maptype = maptype, crop = crop,
        messaging = messaging, urlonly = urlonly, filename = filename, highres = TRUE,
        color = color, api_key = api_key, checkargs = FALSE)
    )
  }    

}