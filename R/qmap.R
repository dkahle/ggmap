#' Quick map plot
#'
#' qmap is a wrapper for \code{\link{ggmapplot}} and \code{\link{ggmap}}.
#' 
#' @param location character; location of interest
#' @param ... stuff to pass to \code{\link{ggmapplot}} and \code{\link{ggmap}}.
#' @return a ggplot object
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{ggmapplot}} and \code{\link{ggmap}}.
#' @export
#' @examples
#'
#' 
#' \dontrun{
#' qmap(location = 'baylor university')
#' qmap(location = 'baylor university', zoom = 14)
#' qmap(location = 'baylor university', zoom = 14, source = 'osm')
#' qmap(location = 'baylor university', zoom = 14, source = 'osm', scale = 20000)
#' qmap(location = 'baylor university', zoom = 14, maptype = 'satellite')
#' qmap(location = 'baylor university', zoom = 14, maptype = 'hybrid')
#' qmap(location = 'baylor university', zoom = 14, maptype = 'toner', source = 'stamen')
#' qmap(location = 'baylor university', zoom = 14, maptype = 'watercolor', source = 'stamen')
#' 
#' wh <- geocode('the white house')
#' qmap('the white house', maprange = TRUE,
#'   base_layer = ggplot(aes(x=lon, y=lat), data = wh)) +
#'   geom_point()
#' 
#' 
#' 
#' 
#' }
#' 
qmap <- function(location = 'houston', ...){
	
  # location formatting
  location_stop <- TRUE
  if(is.character(location) && length(location) == 1){
    location_type <- 'address'
    location_stop <- FALSE    
  }
  if(is.numeric(location) && length(location) == 2){
    location_type <- 'lonlat'
    location_stop <- FALSE      	
  }
  if(is.numeric(location) && length(location) == 4){
    location_type <- 'bbox'
    location_stop <- FALSE      	
  }  
  if(location_stop){
    stop('improper location specification, see ?ggmap.', call. = F)
  }	
	
  args <- as.list(match.call(expand.dots = TRUE)[-1])	
  
  # ggmap args
  ##################  
  
  if('zoom' %in% names(args)){
    zoom <- eval(args$zoom)
  } else {
  	zoom <- 10
  }  
  
  if('scale' %in% names(args)){
    scale <- eval(args$scale)
  } else {
  	scale <- 'auto'
  }    

  if('maptype' %in% names(args)){
    maptype <- eval(args$maptype)
  } else {
    maptype <- 'terrain'
  }  
  
  if('messaging' %in% names(args)){
    messaging <- eval(args$messaging)
  } else {
    messaging <- FALSE
  }       
  
  if('urlonly' %in% names(args)){
    urlonly <- eval(args$urlonly)
  } else {
    urlonly <- FALSE
  }    
  
  if('filename' %in% names(args)){
    filename <- eval(args$filename)
  } else {
    filename <- 'ggmapTemp'
  }    
  
  if('color' %in% names(args)){
    color <- eval(args$color)
  } else {
    color <- 'color'
  }                   
  
  if('source' %in% names(args)){
    source <- eval(args$source)
  } else {
    source <- 'google'
  }        
  
  if('crop' %in% names(args)){
    crop <- eval(args$crop)
  } else {
    crop <- TRUE
  }          
  
  # deprecated
  if(all(c('lonR','latR') %in% names(args))){ 
  	message('lonR and latR arguments deprecated, pass bounding box to location.')  	
  	message('see ?get_openstreetmap.')
    lonR <- eval(args$lonR)
    latR <- eval(args$latR)
    location <- c(left = lonR[1], bottom = latR[1], right = lonR[2], top = latR[2])
  }  
  
  if('type' %in% names(args)){
  	message('type argument deprecated, use color.')
    color <- eval(args$type)
  } else {
    color <- 'color'
  }    
  
  
  # ggmapplot args
  ##################
  
  if('fullpage' %in% names(args)){
    fullpage <- eval(args$fullpage)
  } else {
  	fullpage <- TRUE
  }  
  
  if('maprange' %in% names(args)){
    maprange <- eval(args$maprange)
  } else {
    maprange <- FALSE
  }         
  
  if('base_layer' %in% names(args)){
    base_layer <- args$base_layer
  } else {
    base_layer <- 'auto'
  }              
  
  
  # return
  ggmapplot(
    ggmap(location = location, zoom = zoom, scale = scale, source = source, 
      color = color, maptype = maptype), 
    fullpage = fullpage, maprange = maprange, base_layer = base_layer
  )     
}
