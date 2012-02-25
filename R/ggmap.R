#' Grab a map.
#'
#' ggmap is a smart function which queries the Google Maps server or OpenStreetMap server for a map at a certain location at a certain spatial zoom.
#' 
#' @param location a character string containing the name of the location of interest
#' @param lonR longitude range (only for OpenStreetMaps)
#' @param latR latitude range (only for OpenStreetMaps)
#' @param center named numeric vector of latitude and longitude specifying the center of the image
#' @param regularize logical; should the map grid be regularized?
#' @param type 'color' for a color map, 'bw' for a black and white map
#' @param rgbcoefs when 'bw' is specified, conversions coefficients to use.  see ?ReadImages::rgb2grey
#' @param zoom level of zoom, an integer 0 (whole world) to 19 (higest zoom), default value 10
#' @param maptype any of 'roadmap', 'mobile', 'satellite', 'terrain', 'hybrid', 'mapmaker-roadmap', 'mapmaker-hybrid'
#' @param source 'google' or 'osm' (OpenStreetMaps)
#' @param verbose logical; should function message user?
#' @param destfile character; name of file to save downloaded map
#' @param n_pix numeric; number of pixels in map
#' @param scale numeric; scale of OpenStreetMap, see ?GetMap
#' @param raster logical; use geom_raster?
#' @param ... ...
#' @return a data.frame with columns latitude, longitude, and fill
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{ggmapplot}}, \code{\link{GetMap}} in package RgoogleMaps
#' @export
#' @examples
#'
#' 
#' \dontrun{ 
#' WashingtonMap_df <- ggmap(location = 'washington', verbose = TRUE)
#' str(WashingtonMap_df)
#' ggmapplot(WashingtonMap_df)
#'
#' lonR <- c(-97.12008, -97.11836)
#' latR <- c(31.54765, 31.54911)
#' osm <- ggmap(latR = latR, lonR = lonR, source = 'osm', scale = 1000) 
#' ggmapplot(osm)
#' 
#' }
#' 
ggmap <- function(
  location = 'houston', lonR, latR,
  center = c(lat = 29.7632836, lon = -95.3632715), regularize = TRUE,
  type = c('color','bw'), rgbcoefs = c(0, 1, 0), zoom = 10, 
  maptype = 'terrain', source = c('google', 'osm'), verbose = FALSE,
  destfile = 'ggmapTemp.jpg', n_pix = 640, scale = OSM_scale_lookup(zoom), 
  raster = TRUE, ...
){
  
  type   <- match.arg(type)	
  source <- match.arg(source)
  if(!missing(lonR) && !missing(latR)){
    stopifnot(is.null(c(lonR, latR)) || is.numeric(c(lonR, latR)))
    lonR <- sort(lonR)
    latR <- sort(latR)
  }

  
  # location and url formatting
  if(!missing(location)) center <- geocode(location)	   
	   
  # get google map if desired, otherwise get metadata from google  
  if(verbose) message('grabbing map... ', appendLF = FALSE)
  m <- GetMap(center = center[c('lat','lon')], 
    size = c(n_pix, n_pix), zoom = zoom, format = 'jpg', 
    maptype = maptype, destfile = destfile, verbose = FALSE)
    
    
  # OpenStreetMap?
  if(source == 'osm'){
  	if(missing(lonR) || missing(latR)){
      lonR <- unname(sapply(m$BBOX, function(x) x[2]))
      latR <- unname(sapply(m$BBOX, function(x) x[1]))    
    }
    if(substr(destfile, nchar(destfile)-3, nchar(destfile)) == '.jpg'){
      destfile <- substr(destfile, 1, nchar(destfile) - 4)
      destfile <- paste(destfile, 'png', sep = '.')
    }
    if(substr(destfile, nchar(destfile)-4, nchar(destfile)) == '.jpeg'){
      destfile <- substr(destfile, 1, nchar(destfile) - 5)
      destfile <- paste(destfile, 'png', sep = '.')
    }    
    m <- try(
      GetMap.OSM(lonR = lonR, latR = latR, scale = scale, 
        destfile = destfile, verbose = FALSE), 
      silent = TRUE
    )    
    if(class(m) == 'try-error'){
      stop('map grabbing failed - scale misspecification likely.',
        call. = FALSE)
    }    
  }  
  if(verbose) message('done.')  

  
  # raster?  color map if not
  if(raster){
  	if(type == 'bw') m$myTile <- rgb2grey(m$myTile, coefs = rgbcoefs)
    map <- as.raster(m$myTile)
    attr(map, "bb") <- if(source == 'google'){ data.frame(m$BBOX) } else { 
      data.frame(
        ll.lat = m$BBOX$ll[1], ll.lon = m$BBOX$ll[2], 
        ur.lat = m$BBOX$ur[1], ur.lon = m$BBOX$ur[2]
      )	
    }
    class(map) <- unique(c("ggmap", "raster", class(map)))
    return(map)
  } else {
    if(verbose) message('coloring map... ', appendLF = FALSE) 
    if(type == 'color'){
      map <- apply(m$myTile, 1:2, function(v) rgb(v[1], v[2], v[3], 1, 1, NULL))
    } else if(type == 'bw') {
      nrow <- nrow(m$myTile)
      ncol <- ncol(m$myTile)  	
      map <- grey(rgb2grey(m$myTile, coefs = rgbcoefs))
      map <- matrix(map, nrow = nrow, ncol = ncol)
    }
    if(verbose) message('done.')
  }  

  
  # reshape map for plotting
  if(verbose) message('formatting map... ', appendLF = FALSE)  
  m_map <- reshape2::melt(map)
  names(m_map) <- c('x','y','fill')
  m_map <- within(m_map,{
    x <- x - n_pix/2 - 1
    y <- y - n_pix/2 - 1
  })     
  
  mapInfo <- list(lat = center['lat'], lon = center['lon'], zoom = zoom, map)
  XY_cent <- LatLon2XY.centered(mapInfo, center['lat'], center['lon'])
  #XY2LatLon(HouMapInfo, XY_cent$newX, XY_cent$newY)  
  
  
  # geocode pixel references
  s <- (-n_pix/2) : (n_pix/2 - 1)  
  lat_wrapper <- function(x) XY2LatLon(mapInfo, -n_pix/2, x)[1]
  lats <- apply(data.frame(s), 1, lat_wrapper)  
  lon_wrapper <- function(y) XY2LatLon(mapInfo, y, -n_pix/2)[2]
  lons <- apply(data.frame(s), 1, lon_wrapper)
  
  
  # merge colors to latlons and return
  df_xy   <- expand.grid(x = s, y = s)
  df_ll   <- expand.grid(lat = rev(lats), lon = lons)
  df_xyll <- data.frame(df_xy, df_ll)
  df <- suppressMessages(join(df_xyll, m_map, type = 'right'))
  if(verbose) message('done.')   
  df <- df[,c('lon','lat','fill')]
  class(df) <- c('ggmap','data.frame')
  
  
  # remove missing map points
  if(any(is.na(df))){
    if(verbose) message('removing missing map points... ', appendLF = FALSE)
    df <- df[complete.cases(df),]  	
    if(verbose) message('done.')    
  } 
  
  
  # regularize map grid?
  mapcoords2grid <- function(map){
  	if(verbose) message('regularizing map...', appendLF = FALSE)
    lat_rng  <- range(map$lat)
    lon_rng  <- range(map$lon)
  
    x_pix <- unname(table(map$lat)[1])
    y_pix <- unname(table(map$lon)[1])  
  
    lat_seq <- seq(lat_rng[1], lat_rng[2], length.out = y_pix)
    lon_seq <- seq(lon_rng[1], lon_rng[2], length.out = x_pix)
 
    map[order(map$lon),]$lon <- rep(lon_seq, each = y_pix) 
    map[order(map$lat),]$lat <- rep(lat_seq, each = x_pix)
  	if(verbose) message(' done.\n')    
    map
  }  
  if(regularize) df <- mapcoords2grid(df)    
  
  
  # return
  df
}