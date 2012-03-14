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
#' qmap(location = 'waco')
#' qmap(location = 'waco', zoom = 14)
#' qmap(location = 'waco', zoom = 14, source = 'osm')
#' qmap(location = 'waco', zoom = 14, source = 'osm', scale = 20000)
#' qmap(location = 'waco', zoom = 14, maptype = 'satellite')
#' qmap(location = 'waco', zoom = 14, maptype = 'hybrid')
#' 
#' wh <- geocode('the white house')
#' qmap('the white house', base_layer = ggplot(aes(x=lon, y=lat), data = wh)) +
#'   geom_point()
#' qmap('the white house', maprange = TRUE,
#'   base_layer = ggplot(aes(x=lon, y=lat), data = wh)) +
#'   geom_point()
#' 
#' 
#' 
#' 
#' }
#' 
qmap <- function(location, ...){
  args <- as.list(match.call(expand.dots = TRUE)[-1])	
  
  if('fullpage' %in% names(args)){
    fullpage <- eval(args$fullpage)
  } else {
  	fullpage <- TRUE
  }  
  
  if('zoom' %in% names(args)){
    zoom <- eval(args$zoom)
  } else {
  	zoom <- 10
  }  
  
  if('scale' %in% names(args)){
    scale <- eval(args$scale)
  } else {
  	scale <- OSM_scale_lookup(zoom)
  }    
  
  if('source' %in% names(args)){
    source <- eval(args$source)
  } else {
    source <- 'google'
  }      
  
  if('type' %in% names(args)){
    type <- eval(args$type)
  } else {
    type <- 'color'
  }        
  
  if('maptype' %in% names(args)){
    maptype <- eval(args$maptype)
  } else {
    maptype <- 'terrain'
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

  latlon <- FALSE
  if(all(c('lonR','latR') %in% names(args))){ 
    lonR <- eval(args$lonR)
    latR <- eval(args$latR)
    latlon <- TRUE 
  }
  


  if(latlon){ # osm latlon
  	p <- ggmapplot( 
      ggmap(location = location, zoom = zoom, scale = scale, source = source,
        latR = latR, lonR = lonR, type = type), 
      fullpage = fullpage, maprange = maprange, base_layer = base_layer
    )  	
  } else if(source == 'google'){
  	p <- ggmapplot(
      ggmap(location = location, zoom = zoom, source = source, type = type,
        maptype = maptype), 
      fullpage = fullpage, maprange = maprange, base_layer = base_layer
    )
  } else { # osm zoom
  	p <- ggmapplot(
      ggmap(location = location, zoom = zoom, scale = scale, 
        source = source, type = type), 
      fullpage = fullpage, maprange = maprange, base_layer = base_layer
    )
  }
  
  p
}
