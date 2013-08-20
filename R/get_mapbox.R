#' Get a Mapbox map
#'
#' get_mapbox accesses a tile server for Mapbox and downloads/stiches map tiles/formats a map image.
#' 
#' @param bbox a bounding box in the format c(lowerleftlon, lowerleftlat, upperrightlon, upperrightlat).
#' @param zoom a zoom level
#' @param user_name character string containing Mapbox user name (api key), see details
#' @param maptype a character string representing a unique map style associated with your user name (api_key), see details
#' @param crop crop raw map tiles to specified bounding box
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param filename destination file for download (file extension added according to format)
#' @param color color or black-and-white
#' @param ... ...
#' @details accesses Mapbox maps.  this function requires an api which can be obtained for free from \url{http://www.mapbox.com/}.  
#' thousands of maptypes ("styles"), including create-your-own options, are available.
#' @return a map image as a 2d-array of colors as hexadecimal strings representing pixel fill values.
#' @author David Kahle \email{david.kahle@@gmail.com} extended by Toby Jennings \email{toby@@tobyjennings.com}
#' @seealso \url{http://maps.cloudmade.com/}, \code{\link{ggmap}}
#' @export
#' @examples
#'
#' 
#' \dontrun{ 
#' 	
#' # in what follows, enter your own api key
#' user_name <- '<your api key here>'
#' 
#' map <- get_mapbox()
#' ggmap(map)
#' 
#' map <- get_mapbox(maptype=maptype, user_name = user_name)
#' ggmap(map)
#' 
#' 
#' 
#' 
#' 
#' 
#' }
#' 
get_mapbox <- function(
  bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344), 
  zoom = 10, user_name = 'examples', maptype = 'uci7ul8p', format = c("png","jpeg","jpg"), crop = FALSE, messaging = FALSE, 
  urlonly = FALSE, filename = 'ggmapTemp', color = c('color','bw'), ...
){
	
  # enumerate argument checking (added in lieu of checkargs function)	
  args <- as.list(match.call(expand.dots = TRUE)[-1])  
  argsgiven <- names(args)	
  
  if('bbox' %in% argsgiven){
    if(!(is.numeric(bbox) && length(bbox) == 4)){
      stop('bounding box improperly specified.  see ?get_openstreetmap', call. = F)
    }
  }
   
  if('zoom' %in% argsgiven){    
    if(!(is.numeric(zoom) && length(zoom) == 1 && 
    zoom == round(zoom) && zoom >= 0 && zoom <= 18)){
      stop('scale must be a postive integer 0-18, see ?get_mapbox.', call. = F)
    }    
  }
    
  if('maptype' %in% argsgiven){    
    if(!(is.character(maptype) && length(maptype) < 8 )){
      stop('maptype must be a 8-character string, see ?get_mapbox.', call.=F)  	
    }
  }    
    
  if('user_name' %in% argsgiven){    
    if(!(is.character(user_name) && length(user_name) == 1)){
      stop('user_name improperly specified, see ?get_mapbox.', call.=F)  	
    }
  }     
    
  if('messaging' %in% argsgiven) stopifnot(is.logical(messaging))
    
  if('urlonly' %in% argsgiven) stopifnot(is.logical(urlonly))   

  if('verbose' %in% argsgiven) stopifnot(is.logical(verbose))   
    
  if('filename' %in% argsgiven){
    filename_stop <- TRUE      
    if(is.character(filename) && length(filename) == 1) filename_stop <- FALSE      
    if(filename_stop) stop('improper filename specification, see ?get_googlemap.', call. = F)      
  }        
  
  if('pngColors' %in% argsgiven){
    if( is.numeric(pngColors) && ! pngColors %in% c(32,64,128,256) ){
      stop('pngColors must be one of 32, 64, 128, or 256', call. = F)
    }
    if( ! format=="png" ){
      stop('pngColors only compatible with png format', call. = F)
    }
  }
  
  if('jpgQuality' %in% argsgiven){
    if( is.numeric(jpgQuality) && ! jpgQuality %in% c(70,80,90) ){
      stop('jpgQuality must be one of 70, 80, or 90', call. = F)
    }
    if( ! format %in% c("jpg","jpeg")){
      stop('jpgQuality only compatible with jpg format', call. = F)
    }
  }
  # color arg checked by match.arg 
  
  # format arg checked by match.arg
  
  if('checkargs' %in% argsgiven){
    .Deprecated(msg = 'checkargs argument deprecated, args are always checked after v2.1.')
  }  
      
  
  # argument checking (no checks for language, region, markers, path, visible, style)
  #args <- as.list(match.call(expand.dots = TRUE)[-1])  
  #if(checkargs) get_mapbox_checkargs(args) 
  color <- match.arg(color)
  format <- match.arg(format)
  if(is.null(names(bbox))) names(bbox) <- c('left','bottom','right','top')

  # determine tiles to get
  fourCorners <- expand.grid(
    lon = c(bbox['left'], bbox['right']), 
    lat = c(bbox['bottom'], bbox['top'])
  )
  fourCorners$zoom <- zoom
  row.names(fourCorners) <- 
    c('lowerleft','lowerright','upperleft','upperright')  
  fourCornersTiles <- apply(fourCorners, 1, function(v) LonLat2XY(v[1],v[2],v[3]))  

  xsNeeded <- Reduce(':', sort(unique(as.numeric(sapply(fourCornersTiles, function(df) df$X)))))
  numXTiles <- length(xsNeeded)
  ysNeeded <- Reduce(':', sort(unique(as.numeric(sapply(fourCornersTiles, function(df) df$Y)))))
  numYTiles <- length(ysNeeded)  
  tilesNeeded <- expand.grid(x = xsNeeded, y = ysNeeded)
  if(nrow(tilesNeeded) > 40){
    message(paste0(nrow(tilesNeeded), ' tiles needed, this may take a while ',
      '(try a smaller zoom).'))
  }
  xTileProgression <- rep(1:numXTiles, numYTiles)  
  yTileProgression <- rep(1:numYTiles, each = numXTiles)
  
  
  # make urls
  base_url <- 'http://a.tiles.mapbox.com/v3/'
  base_url <- paste(base_url, user_name, '.map-', maptype, sep = '')
  base_url <- paste(base_url, zoom, sep = '/')
  urls <- paste(base_url, 
    apply(tilesNeeded, 1, paste, collapse = '/'), sep = '/')
  if(format=="png") {
    urls <- paste(urls, '.png', sep = '')
  } else {
    urls <- paste(urls, '.jpg', sep = '')
  }
  if(pngColors) urls <- paste(urls, pngColors, sep='')
  if(jpgQuality) urls <- paste(urls, jpgQuality, sep='')
  if(messaging) message(length(urls), ' tiles required.')
  if(urlonly) return(urls)

  # download and stitch
  size <- 256 * c(length(xsNeeded), length(ysNeeded))  
  map <- matrix('NA', nrow = size[2], ncol = size[1])
  destfile <- paste(filename, 'png', sep = '.')
  
  for(k in seq_along(urls)){
    if (messaging) print(paste("Fetching tile at",urls[[k]],sep=" "))
    download.file(urls[[k]], destfile = destfile, quiet = !messaging, mode = 'wb')
    # Mapbox can deliver a PNG or a JPEG depending on the type of tile customization being
    # downloaded. They are always _named_ PNG, though, if you don't specify a format.
    tile <- tryCatch( {readPNG(destfile)}, error=function(e){readJPEG(destfile)} )
    if(color == 'color'){
      tile <- apply(tile, 2, rgb)
    } else if(color == 'bw'){
      tile_dim <- dim(tile)
  	  tile <- gray(.30 * tile[,,1] + .59 * tile[,,2] + .11 * tile[,,3])
      dim(tile) <- tile_dim[1:2]
    }    

    map[
      (1+256*(yTileProgression[k]-1)):(256*yTileProgression[k]),
      (1+256*(xTileProgression[k]-1)):(256*xTileProgression[k])
    ] <- tile
  }
  
  # determine bbox of map. note : not the same as the argument bounding box -
  # the map is only a covering of the bounding box extent the idea is to get
  # the lower left tile and the upper right tile and compute their bounding boxes
  # tiles are referenced by top left of tile, starting at 0,0
  # see http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  bboxOfTile <- function(vXY){
    lonlat_upperleft <- XY2LonLat(vXY[1],vXY[2],zoom)  
    lonlat_lowerright <- XY2LonLat(vXY[1]+1,vXY[2]+1,zoom)
    data.frame(
      left = lonlat_upperleft$lon,
      bottom = lonlat_lowerright$lat,
      right = lonlat_lowerright$lon,
      top = lonlat_upperleft$lat
    )
  }  
  tileBboxes <- ldply(split(tilesNeeded,1:nrow(tilesNeeded)), 
    function(df) bboxOfTile(as.numeric(df)))
  mbbox <- c(
    left = min(tileBboxes$left),
    bottom = min(tileBboxes$bottom),    
    right = max(tileBboxes$right),
    top = max(tileBboxes$top)
  )

  
  # format map and return if not cropping
  if(!crop){  
    map <- as.raster(map)
    class(map) <- c('ggmap','raster')
    attr(map, 'bb') <- data.frame(
      ll.lat = mbbox['bottom'], ll.lon = mbbox['left'],
      ur.lat = mbbox['top'], ur.lon = mbbox['right']
    )  
    return(map)
  }


  # crop map  
  if(crop){
    slon <- seq(mbbox['left'], mbbox['right'], length.out = size[1])
    slat <- seq(mbbox['top'], mbbox['bottom'], length.out = size[2])    

    keep_x_ndcs <- which(bbox['left'] <= slon & slon <= bbox['right'])
    keep_y_ndcs <- which(bbox['bottom'] <= slat & slat <= bbox['top'])    
    
    croppedmap <- map[keep_y_ndcs, keep_x_ndcs]
  }
  
  # format map
  croppedmap <- as.raster(croppedmap)  
  class(croppedmap) <- c('ggmap','raster')
  attr(croppedmap, 'bb') <- data.frame(
    ll.lat = bbox['bottom'], ll.lon = bbox['left'],
    ur.lat = bbox['top'], ur.lon = bbox['right']
  )    

  
  # return
  croppedmap
}


















get_mapbox_checkargs <- function(args){
  eargs <- lapply(args, eval)
  argsgiven <- names(args)

  with(eargs,{
  	
    # bbox arg
    if('bbox' %in% argsgiven){
      if(!(is.numeric(bbox) && length(bbox) == 4)){
        stop('bounding box improperly specified.  see ?get_openstreetmap', call. = F)
      }
    }
   
    # zoom arg
    if('zoom' %in% argsgiven){    
      if(!(is.numeric(zoom) && length(zoom) == 1 && 
      zoom == round(zoom) && zoom >= 0 && zoom <= 18)){
        stop('scale must be a postive integer 0-18, see ?get_stamenmap.', call. = F)
      }    
    }
    
    # maptype arg
    if('maptype' %in% argsgiven){    
      if(!(is.numeric(maptype) && length(maptype) == 1 && 
          maptype == round(maptype) && maptype > 0)){
        stop('maptype must be a positive integer, see ?get_mapbox.', call.=F)  	
      }
    }    
    
    # user_name arg
    if('user_name' %in% argsgiven){    
      if(!(is.character(user_name) && length(user_name) == 1)){
        stop('user_name improperly specified, see ?get_mapbox.', call.=F)  	
      }
    } else {
      stop('user_name must be specified, see ?get_mapbox.')
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
      if(is.character(filename) && length(filename) == 1) style_stop <- FALSE      
      if(filename_stop) stop('improper filename specification, see ?get_googlemap.', call. = F)      
    }        
    
    # color arg checked by match.arg    
        
  
  }) # end with
}



