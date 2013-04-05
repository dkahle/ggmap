#' Get a Google Map
#'
#' get_googlemap accesses the Google Static Maps API version 2 to download a static map.  Note that in most cases by using this function you are agreeing to the Google Maps API Terms of Service at https://developers.google.com/maps/terms.
#' 
#' @param center the center of the map.  this can either be 1. a longitude/latitude numeric vector or 2. a character string address (note that the latter uses a geocode)
#' @param zoom map zoom, an integer from 3 (continent) to 21 (building), default value 10 (city)
#' @param size rectangular dimensions of map in pixels - horizontal x vertical - with a max of c(640, 640).  this parameter is affected in a multiplicative way by scale.
#' @param scale multiplicative factor for the number of pixels returned possible values are 1, 2, or 4 (e.g. size = c(640,640) and scale = 2 returns an image with 1280x1280 pixels).  4 is reserved for google business users only.  scale also affects the size of labels as well.
#' @param format character string providing image format - png, jpeg, and gif formats available in various flavors
#' @param maptype character string providing google map theme. options available are 'terrain', 'satellite', 'roadmap', and 'hybrid'
#' @param language character string providing language of map labels (for themes with them) in the format 'en-EN'.  not all languages are supported; for those which aren't the default language is used
#' @param region borders to display as a region code specified as a two-character ccTLD ('top-level domain') value, see \url{http://en.wikipedia.org/wiki/List_of_Internet_top-level_domains#Country_code_top-level_domains}
#' @param markers data.frame with first column longitude, second column latitude, for which google markers should be embedded in the map image, or character string to be passed directly to api
#' @param path data.frame (or list of data.frames) with first column longitude, second column latitude, for which a single path should be embedded in the map image, or character string to be passed directly to api
#' @param visible a location as a longitude/latitude numeric vector (or data frame with first column longitude, second latitude) or vector of character string addresses which should be visible in map extent
#' @param style character string to be supplied directly to the api for the style argument .  this is a powerful complex specification, see \url{https://developers.google.com/maps/documentation/staticmaps/}
#' @param sensor specifies whether the application requesting the static map is using a sensor to determine the user's location
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param filename destination file for download (file extension added according to format)
#' @param color color or black-and-white
#' @return a map image as a 2d-array of colors as hexadecimal strings representing pixel fill values.
#' @param ... ...
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \url{https://developers.google.com/maps/documentation/staticmaps/}, \code{\link{ggmap}}
#' @export
#' @examples
#'
#' 
#' \dontrun{ 
#' 	
#' get_googlemap(urlonly = TRUE)
#' 
#' # get_googlemap has several argument checks
#' get_googlemap(zoom = 13.5)
#' get_googlemap(scale = 3)
#' get_googlemap(center = c(-30,-110))
#' 
#' # markers and paths are easy to access
#' d <- function(x=-95.36, y=29.76, n,r,a){
#'   round(data.frame(
#'     lon = jitter(rep(x,n), amount = a),
#'     lat = jitter(rep(y,n), amount = a)
#'   ), digits = r)
#' }
#' df <- d(n=50,r=3,a=.3)
#' map <- get_googlemap(markers = df, path = df,, scale = 2)
#' ggmap(map)
#' ggmap(map, fullpage = TRUE) + 
#'   geom_point(aes(x = lon, y = lat), data = df, size = 3, colour = 'black') +
#'   geom_path(aes(x = lon, y = lat), data = df)
#' 
#' gc <- geocode('waco, texas')
#' center <- as.numeric(gc)
#' ggmap(get_googlemap(center = center, color = 'bw', scale = 2), fullpage = T)
#' 
#' # the scale argument can be seen in the following
#' # (make your graphics device as large as possible)
#' ggmap(get_googlemap(center, scale = 1), fullpage = TRUE) # pixelated
#' ggmap(get_googlemap(center, scale = 2), fullpage = TRUE) # fine
#' 
#' 
#' }
#' 
get_googlemap <- function(
  center = c(lon = -95.3632715, lat = 29.7632836), zoom = 10, size = c(640,640), 
  scale = 2, format = c('png8', 'gif', 'jpg', 'jpg-baseline','png32'), 
  maptype = c('terrain', 'satellite', 'roadmap', 'hybrid'), 
  language = 'en-EN', region, markers, path, visible, style, sensor = FALSE,
  messaging = FALSE, urlonly = FALSE, filename = 'ggmapTemp', color = c('color','bw'), ...
){
	
  # enumerate argument checking (added in lieu of checkargs function)	
  args <- as.list(match.call(expand.dots = TRUE)[-1])  
  argsgiven <- names(args)
  
  if('center' %in% argsgiven){     
    if(!(
      (is.numeric(center) && length(center) == 2) || 
      (is.character(center) && length(center) == 1)
    )){
      stop('center of map misspecified, see ?get_googlemap.', call. = F)
    }
    if(all(is.numeric(center))){ 
      lon <- center[1]; lat <- center[2]
      if(lon < -180 || lon > 180){
        stop('longitude of center must be between -180 and 180 degrees.',
          ' note ggmap uses lon/lat, not lat/lon.', call. = F)
      }
      if(lat < -90 || lat > 90){
        stop('latitude of center must be between -90 and 90 degrees.',
          ' note ggmap uses lon/lat, not lat/lon.', call. = F)
      }    
    }
  }
 
  if('zoom' %in% argsgiven){
    if(!(is.numeric(zoom) && zoom == round(zoom) && zoom > 0)){
      stop('zoom must be a whole number between 1 and 21', call. = F)
    }  
  }
  
  if('size' %in% argsgiven){    
    stopifnot(all(is.numeric(size)) && all(size == round(size)) && all(size > 0))
  }
    
  if('scale' %in% argsgiven){    
    stopifnot(scale %in% c(1,2,4))
  }
  
  # format arg checked by match.arg
  
  # maptype arg checked by match.arg
    
  if('markers' %in% argsgiven){
    markers_stop <- TRUE
    if(is.data.frame(markers) && all(apply(markers[,1:2],2,is.numeric))) markers_stop <- FALSE
    if(
      class(markers) == 'list' && 
      all(sapply(markers, function(elem){
        is.data.frame(elem) && all(apply(elem[,1:2],2,is.numeric))
      })) 
    ) markers_stop <- FALSE
    if(is.character(markers) && length(markers) == 1) markers_stop <- FALSE
      
    if(markers_stop) stop('improper marker specification, see ?get_googlemap.', call. = F)
  }
    
  if('path' %in% argsgiven){    
    path_stop <- TRUE
    if(is.data.frame(path) && all(apply(path[,1:2],2,is.numeric))) path_stop <- FALSE
    if(
      class(path) == 'list' && 
      all(sapply(path, function(elem){
        is.data.frame(elem) && all(apply(elem[,1:2],2,is.numeric))
      })) 
    ) path_stop <- FALSE
    if(is.character(path) && length(path) == 1) path_stop <- FALSE
    
    if(path_stop) stop('improper path specification, see ?get_googlemap.', call. = F)
  }    
    
  if('visible' %in% argsgiven){ 
    message('visible argument untested.')
    visible_stop <- TRUE      
    if(is.data.frame(visible) && all(apply(visible[,1:2],2,is.numeric))) visible_stop <- FALSE
    if(is.character(visible)) visible_stop <- FALSE
    if(visible_stop) stop('improper visible specification, see ?get_googlemap.', call. = F)      
  }
 
  if('style' %in% argsgiven){
    message('style argument untested.')    	
    style_stop <- TRUE      
    if(is.character(style) && length(style) == 1) style_stop <- FALSE      
    if(style_stop) stop('improper style specification, see ?get_googlemap.', call. = F)      
  }
      
  if('sensor' %in% argsgiven) stopifnot(is.logical(sensor))  
    
  if('messaging' %in% argsgiven) stopifnot(is.logical(messaging))
    
  if('urlonly' %in% argsgiven) stopifnot(is.logical(urlonly))
    
  if('filename' %in% argsgiven){
    filename_stop <- TRUE      
    if(is.character(filename) && length(filename) == 1) filename_stop <- FALSE      
    if(filename_stop) stop('improper filename specification, see ?get_googlemap.', call. = F)      
  }      
  
  if('checkargs' %in% argsgiven){
    .Deprecated(msg = 'checkargs argument deprecated, args are always checked after v2.1.')
  }
  
	
  
  # argument checking (no checks for language, region, markers, path, visible, style)
  #args <- as.list(match.call(expand.dots = TRUE)[-1])  
  #if(checkargs) get_googlemap_checkargs(args)
  format <- match.arg(format)
  if(format != 'png8') stop('currently only the png format is supported.', call. = F)
  maptype <- match.arg(maptype)
  color <- match.arg(color)  
  if(!missing(markers) && class(markers) == 'list') markers <- plyr:::list_to_dataframe(markers)
  if(!missing(path) && is.data.frame(path)) path <- list(path)

  
  
  # url segments
  base_url <- 'http://maps.googleapis.com/maps/api/staticmap?'
  center_url <- if(all(is.numeric(center))){ # lon/lat specification
    center <- round(center, digits = 6)
    lon <- center[1]; lat <- center[2]
    paste('center=', paste(lat,lon,sep = ','), sep = '')
  } else { # address specification
    centerPlus <- gsub(' ', '+', center)
    paste('center=', centerPlus, sep = '')    
  }
  zoom_url <- paste('zoom=', zoom, sep = '')
  size_url <- paste('size=', paste(size, collapse='x'))
  scale_url <- if(!missing(scale)){ paste('scale=', scale) } else { '' }
  format_url <- if(!missing(format) && format != 'png8'){ paste('format=', format) } else { '' }
  maptype_url <- paste('maptype=', maptype, sep = '')
  language_url <- if(!missing(language)){ paste('language=', language) } else { '' }  
  region_url <- if(!missing(region)){ paste('region=', region) } else { '' }
  
  markers_url <- 
  if(!missing(markers)){
  	if(is.data.frame(markers)){
      paste('markers=', 
        paste(
          apply(markers, 1, function(v) paste(rev(round(v,6)), collapse = ',')), 
        collapse = '|'),
        sep = ''
      )      
    } else {
      paste('markers=', markers, sep = '')
    } 
  } else { '' }
  
  path_url <- 
  if(!missing(path)){
    if(is.list(path)){
    	ps <- sapply(path, function(one_path){
        paste('path=', 
          paste(
            apply(one_path, 1, function(v) paste(rev(round(v,6)), collapse = ',')), 
          collapse = '|'),
          sep = ''
        )   	  
    	})
    	paste(ps, collapse = '&')  
    } else {
      paste('path=', path, sep = '')
    }
  } else { '' }
  
  visible_url <- 
  if(!missing(visible)){
    if(is.data.frame(visible)){
      paste('visible=', 
        paste(
          apply(visible, 1, function(v) paste(rev(round(v,6)), collapse = ',')), 
        collapse = '|'),
        sep = ''
      )      
    } else {
      paste('visible=', paste(visible, collapse = '|'), sep = '')
    } 
  } else { '' }
  
  style_url <- if(!missing(style)){ paste('style=', style) } else { '' }
  sensor_url <- paste('sensor=', tolower(as.character(sensor)), sep='')
  
  
  # format url proper
  post_url <- paste(center_url, zoom_url, size_url, scale_url, 
    format_url, maptype_url, language_url, region_url, markers_url, 
    path_url, visible_url, style_url, sensor_url, sep = '&')
  url <- paste(base_url, post_url, sep = '')
  url <- gsub('[&]+','&',url) # removes missing arguments
  if(substr(url, nchar(url), nchar(url)) == '&'){ # if ends with &
    url <- substr(url, 1, nchar(url)-1) 
  }
  url <- URLencode(url)
  if(urlonly) return(url)
  if(nchar(url) > 2048) stop('max url length is 2048 characters.', call. = FALSE)

  # read in file
  destfile <- if(format %in% c('png8','png32')){
    paste(filename,'png',sep = '.')
  } else if(format %in% c('jpg','jpg-baseline')){
  	paste(filename,'jpg',sep = '.')
  } else {
  	paste(filename,'gif',sep = '.')
  }
  download.file(url, destfile = destfile, quiet = !messaging, mode = 'wb')
  message(paste0('Map from URL : ', url))
  message('Google Maps API Terms of Service : http://developers.google.com/maps/terms')
  map <- readPNG(destfile)
  
  # format file
  if(color == 'color'){
    map <- apply(map, 2, rgb)
  } else if(color == 'bw'){
  	mapd <- dim(map)
  	map <- gray(.30 * map[,,1] + .59 * map[,,2] + .11 * map[,,3])
  	dim(map) <- mapd[1:2]
  }
  class(map) <- c('ggmap','raster')
  
  # map spatial info
  if(is.character(center)) center <- as.numeric(geocode(center))
  ll <- XY2LatLon(
    list(lat = center[2], lon = center[1], zoom = zoom),
    -size[1]/2 + 0.5,
    -size[2]/2 - 0.5
  )
  ur <- XY2LatLon(
    list(lat = center[2], lon = center[1], zoom = zoom),
    size[1]/2 + 0.5,
    size[2]/2 - 0.5
  )  
  attr(map, 'bb') <- data.frame(
    ll.lat = ll[1], ll.lon = ll[2],
    ur.lat = ur[1], ur.lon = ur[2]
  )
  
  # return
  t(map)
}




















get_googlemap_checkargs <- function(args){
  eargs <- lapply(args, eval)
  argsgiven <- names(args)

  with(eargs,{
  	
    # center arg
    if('center' %in% argsgiven){     
      if(!(
        (is.numeric(center) && length(center) == 2) || 
        (is.character(center) && length(center) == 1)
      )){
        stop('center of map misspecified, see ?get_googlemap.', call. = F)
      }
      if(all(is.numeric(center))){ 
        lon <- center[1]; lat <- center[2]
        if(lon < -180 || lon > 180){
          stop('longitude of center must be between -180 and 180 degrees.',
            ' note ggmap uses lon/lat, not lat/lon.', call. = F)
        }
        if(lat < -90 || lat > 90){
          stop('latitude of center must be between -90 and 90 degrees.',
            ' note ggmap uses lon/lat, not lat/lon.', call. = F)
        }    
      }
    }
  
    # zoom arg
    if('zoom' %in% argsgiven){
      if(!(is.numeric(zoom) && zoom == round(zoom) && zoom > 0)){
        stop('zoom must be a whole number between 1 and 21', call. = F)
      }  
    }
  
    # size arg
    if('size' %in% argsgiven){    
      stopifnot(all(is.numeric(size)) && all(size == round(size)) && all(size > 0))
    }
    
    # scale arg
    if('scale' %in% argsgiven){    
      stopifnot(scale %in% c(1,2,4))
    }
  
    # format arg checked by match.arg
  
    # maptype arg checked by match.arg
    
    # markers arg (optional)
    if('markers' %in% argsgiven){
      markers_stop <- TRUE
      if(is.data.frame(markers) && all(apply(markers[,1:2],2,is.numeric))) markers_stop <- FALSE
      if(
        class(markers) == 'list' && 
        all(sapply(markers, function(elem){
          is.data.frame(elem) && all(apply(elem[,1:2],2,is.numeric))
        })) 
      ) markers_stop <- FALSE
      if(is.character(markers) && length(markers) == 1) markers_stop <- FALSE
      
      if(markers_stop) stop('improper marker specification, see ?get_googlemap.', call. = F)
    }
    
    # path arg (optional)
    if('path' %in% argsgiven){    
      path_stop <- TRUE
      if(is.data.frame(path) && all(apply(path[,1:2],2,is.numeric))) path_stop <- FALSE
      if(
        class(path) == 'list' && 
        all(sapply(path, function(elem){
          is.data.frame(elem) && all(apply(elem[,1:2],2,is.numeric))
        })) 
      ) path_stop <- FALSE
      if(is.character(path) && length(path) == 1) path_stop <- FALSE
      
      if(path_stop) stop('improper path specification, see ?get_googlemap.', call. = F)
    }    
    
    # visible arg (optional)
    if('visible' %in% argsgiven){ 
      message('visible argument untested.')
      visible_stop <- TRUE      
      if(is.data.frame(visible) && all(apply(visible[,1:2],2,is.numeric))) visible_stop <- FALSE
      if(is.character(visible)) visible_stop <- FALSE
      if(visible_stop) stop('improper visible specification, see ?get_googlemap.', call. = F)      
    }
 
    # style arg (optional)
    if('style' %in% argsgiven){
      message('style argument untested.')    	
      style_stop <- TRUE      
      if(is.character(style) && length(style) == 1) style_stop <- FALSE      
      if(style_stop) stop('improper style specification, see ?get_googlemap.', call. = F)      
    }
      
    # sensor arg
    if('sensor' %in% argsgiven){    
      stopifnot(is.logical(sensor))  
    }
    
    # messaging arg
    if('messaging' %in% argsgiven){    
      stopifnot(is.logical(messaging))      
    }
    
    # urlonly arg
    if('urlonly' %in% argsgiven){    
      stopifnot(is.logical(urlonly))      
    }    
    
    # filename arg 
    if('filename' %in% argsgiven){
      filename_stop <- TRUE      
      if(is.character(filename) && length(filename) == 1) filename_stop <- FALSE      
      if(filename_stop) stop('improper filename specification, see ?get_googlemap.', call. = F)      
    }    
    
    # color arg checked by match.arg    
  
  }) # end with
}