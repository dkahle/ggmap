#' Grab a route from Google
#'
#' Grab a route from Google
#' 
#' @param from name of origin addresses in a data frame (vector accepted)
#' @param to name of destination addresses in a data frame (vector accepted)
#' @param output amount of output
#' @param mode driving, bicycling, or walking
#' @param alternatives should more than one route be provided?
#' @param messaging turn messaging on/off
#' @param sensor whether or not the geocoding request comes from a device with a location sensor
#' @param override_limit override the current query count (.GoogleRouteQueryCount)
#' @return a data frame (output='simple') or all of the geocoded information (output='all')
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \url{https://developers.google.com/maps/documentation/directions/}
#' @export
#' @examples
#'
#' 
#' \dontrun{
#' 
#' from <- 'houson, texas'
#' to <- 'waco, texas'
#' route_df <- route(from, to)
#' qmap('college station, texas', zoom = 8) +
#'   geom_segment(
#'     aes(x = startLon, y = startLat, xend = endLon, yend = endLat), 
#'     colour = 'red', size = 1.5, data = route_df
#'   )
#' 
#' qmap('college station, texas', zoom = 6) +
#'   geom_segment(
#'     aes(x = startLon, y = startLat, xend = endLon, yend = endLat), 
#'     colour = 'red', size = 1.5, data = route_df
#'   )
#'
#' theme_set(theme_bw())
#' route_df <- route(from, to, alternatives = TRUE)
#' p <- qplot(route, minutes, data = route_df, geom = 'bar', 
#'     stat = 'identity', fill = factor(leg)) +
#'   scale_fill_discrete(guide = 'none') +
#'   labs(x = 'Route', y = 'Time (Minutes)', fill = 'Leg') +
#'   opts(
#'     title = 'Route Time by Leg', 
#'     plot.background = theme_rect(fill = 'green'),
#'     axis.text.x = theme_text(colour = 'white'),
#'     axis.title.x = theme_text(colour = 'white'),    
#'     axis.text.y = theme_text(colour = 'white'),
#'     axis.title.y = theme_text(colour = 'white', angle = 90),
#'     plot.title = theme_text(colour = 'white')
#'   )
#' 
#' options('device')$device(width = 7.56, height = 6.84)  
#' qmap('college station, texas', zoom = 8, maptype = 'satellite', fullpage = FALSE) +
#'   geom_segment(
#'     aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = route), 
#'     alpha = 3/4, size = 1.75, data = route_df
#'   ) +
#'   labs(x = 'Longitude', y = 'Latitude', colour = 'Routes') +
#'   opts(title = 'Approximate Routes from Houston to Waco') +
#'   ggmap:::annotation_custom(ggplotGrob(p), 
#'     xmin = -96.5, xmax = -94.5, ymin = 30.35, ymax = 32.2)
#'   
#' routeQueryCheck()
#'   
#' 
#' 
#' }
#' 
route <- function(from, to, mode = c('driving','walking','bicycling'), 
  output = c('simple','all'), alternatives = FALSE, messaging = FALSE, sensor = TRUE,
  override_limit = FALSE)
{
  require(rjson)
  require(plyr)
	
  # check parameters
  if(is.numeric(from) && length(from) == 2) from <- revgeocode(from)
  stopifnot(is.character(from))
  if(is.numeric(to) && length(to) == 2) to <- revgeocode(to)
  stopifnot(is.character(to))  
  mode <- match.arg(mode)    
  output <- match.arg(output)  
  stopifnot(is.logical(alternatives))  
  stopifnot(is.logical(messaging))
  stopifnot(is.logical(sensor))  
  
  # format url
  origin <- from
  origin <- gsub(' ', '', origin)
  origin <- paste('origin=', origin, sep = '')      
  destination <- to
  destination <- gsub(' ', '', destination)  
  destination <- paste('destination=', destination, sep = '')    
  mode4url <- paste('mode=', mode, sep = '')   
  unit4url <- paste('units=', 'metric', sep = '')       
  alts4url <- paste('alternatives=', tolower(as.character(alternatives)), sep = '')     
  sensor4url <- paste('sensor=', tolower(as.character(sensor)), sep = '')   
  posturl <- paste(origin, destination, mode4url, unit4url, alts4url, sensor4url, sep = '&')    
  url_string <- paste("http://maps.googleapis.com/maps/api/directions/json?", 
    posturl, sep = "")
  url_string <- URLencode(url_string)
    
  # check/update google query limit
  check_route_query_limit(url_string, elem = 1, 
    override = override_limit, messaging = messaging)    

  
  # distance lookup
  if(messaging) message('trying url ', url_string)
  tree <- fromJSON(paste(readLines(url(url_string)), collapse = ''))
  closeAllConnections()  
  
  # return output = 'all'
  if(output == 'all') return(tree)
  
  
  
  
  
  # extract output from tree and format
  out <- ldply(tree$routes, function(route){
  
    route_df <- ldply(route$legs[[1]]$steps, function(oneLegList){
      data.frame(
        m = oneLegList$distance$value,
        km = oneLegList$distance$value/1000,
        miles = 0.0006214 * oneLegList$distance$value,
        seconds = oneLegList$duration$value,
        minutes = oneLegList$duration$value / 60,
  	    hours = oneLegList$duration$value / 3600,
  	    startLon = oneLegList$start_location$lng, 
  	    startLat = oneLegList$start_location$lat,
  	    endLon = oneLegList$end_location$lng, 
  	    endLat = oneLegList$end_location$lat
      )    
    }) 
    route_df$leg <- 1:nrow(route_df) 
    route_df
  })
  
  # label routes
  stepsPerRoute <- 
    sapply(tree$routes, function(route) length(route$legs[[1]]$steps))  
  
  nRoutes <- length(stepsPerRoute)
  routeLabel <- NULL
  for(k in 1:nRoutes){
    routeLabel <- c(routeLabel, rep(LETTERS[k], stepsPerRoute[k]))	
  }
  if(nRoutes > 1) out$route <- routeLabel

  # return output = 'simple'  
  out
}



check_route_query_limit <- function(url_string, elems, override, messaging){  
  .GoogleRouteQueryCount <- NULL; rm(.GoogleRouteQueryCount); # R CMD check trick
  	
  if(exists('.GoogleRouteQueryCount', .GlobalEnv)){
    	
    .GoogleRouteQueryCount <<- 
      subset(.GoogleRouteQueryCount, time >= Sys.time() - 24*60*60)
    
    # 2500 per 24 hours
    if(sum(.GoogleRouteQueryCount$elements) + elems > 2500){
      message('query max exceeded, see ?mapdist.  current total = ', 
        sum(.GoogleRouteQueryCount$elements))
      if(!override) stop('google query limit exceeded.', call. = FALSE)
    }
    
    # 100 per 10 seconds
    if(with(.GoogleRouteQueryCount, 
      sum(elements[time >= Sys.time() - 10]) + elems > 100
    )){
      if(messaging) message('waiting 10 seconds for another 100 queries...', appendLF=F)
      Sys.sleep(10) # can do better
      if(messaging) message(' done')      
    }    
      
    # append to .GoogleRouteQueryCount
    .GoogleRouteQueryCount <<- rbind(.GoogleRouteQueryCount, 
      data.frame(time = Sys.time(),  url = url_string, 
        elements = elems, stringsAsFactors = FALSE)
    )
    
    	
  } else {
    	
    .GoogleRouteQueryCount <<- 
      data.frame(time = Sys.time(),  url = url_string, 
        elements = elems, stringsAsFactors = FALSE)
      
  }
}


#' Check Google Maps Directions API query limit
#'
#' Check Google Maps Directions API query limit
#' 
#' @return a data frame
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \url{https://developers.google.com/maps/documentation/directions/}
#' @export
#' @examples
#' routeQueryCheck()
routeQueryCheck <- function(){
  .GoogleRouteQueryCount <- NULL; rm(.GoogleRouteQueryCount); # R CMD check trick	
  if(exists('.GoogleRouteQueryCount', .GlobalEnv)){    	
  	remaining <- 2500-sum(
  	  subset(.GoogleRouteQueryCount, time >= Sys.time() - 24*60*60)$elements
  	  )
    message(remaining, ' route queries remaining.')
  } else {
  	remaining <- 2500
    message(remaining, ' route queries remaining.')
  }	
  invisible(remaining)
}