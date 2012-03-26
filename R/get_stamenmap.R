#' Get a Stamen Map.
#'
#' get_stamenmap accesses a tile server for Stamen Maps and downloads/stiches map tiles/formats a map image.
#' 
#' @param bbox a bounding box in the format c(lowerleftlon, lowerleftlat, upperrightlon, upperrightlat).
#' @param zoom a zoom level
#' @param maptype terrain, watercolor, or toner
#' @param crop crop raw map tiles to specified bounding box
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param filename destination file for download (file extension added according to format)
#' @param color color or black-and-white
#' @param checkargs check arguments
#' @details accesses stamen maps.
#' @return a map image as a 2d-array of colors as hexadecimal strings representing pixel fill values.
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \url{http://maps.stamen.com/#watercolor}, \code{\link{ggmapplot}}
#' @export
#' @examples
#'
#' 
#' \dontrun{ 
#' 	
#' gc <- geocode('rice university')	
#' stamen <- get_stamenmap(crop = TRUE)
#' ggmapplot(stamen) +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 3)
#' 
#' osm <- get_openstreetmap(checkargs = FALSE)
#' ggmapplot(osm) +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 3)
#' 
#' google <- get_googlemap(checkargs = FALSE)
#' ggmapplot(google) +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 3)
#' 
#' ggmapplot(get_stamenmap(maptype = 'watercolor')) 
#' ggmapplot(get_stamenmap(maptype = 'watercolor', color = 'bw')) 
#'
#' ggmapplot(get_stamenmap(maptype = 'toner'))  
#' 
#' }
#' 
get_stamenmap <- function(
  bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344), 
  zoom = 10, maptype = c('terrain','watercolor','toner'), crop = TRUE, messaging = FALSE, 
  urlonly = FALSE, filename = 'ggmapTemp', color = c('color','bw'), checkargs = TRUE
){
  
  # argument checking (no checks for language, region, markers, path, visible, style)
  args <- as.list(match.call(expand.dots = TRUE)[-1])  
  if(checkargs) get_stamenmap_checkargs(args)
  maptype <- match.arg(maptype)    
  color <- match.arg(color)  
  if(is.null(names(bbox))) names(bbox) <- c('left','bottom','right','top')

  # determine tiles to get
  fourCorners <- expand.grid(
    lon = c(bbox['left'], bbox['right']), 
    lat = c(bbox['bottom'], bbox['top'])
  )
  row.names(fourCorners) <- c('lowerleft', 'lowerright', 'upperleft', 'upperright')
  fourCorners$zoom <- zoom
  fourCornerTiles <- t(apply(fourCorners, 1, function(v){
    LatLon2XY(v[2],v[1],v[3])$Tile
  }))
  fourCornerTiles <- as.data.frame(fourCornerTiles)
  names(fourCornerTiles) <- c('x','y')
  xsNeeded <- Reduce(':', sort(unique(fourCornerTiles$x)))
  xsNeeded <- c(xsNeeded, max(xsNeeded)+1)  
  numXTiles <- length(xsNeeded)
  ysNeeded <- Reduce(':', sort(unique(fourCornerTiles$y)))
  ysNeeded <- c(min(ysNeeded)-1, ysNeeded)
  numYTiles <- length(ysNeeded)  
  tilesNeeded <- expand.grid(x = xsNeeded, y = ysNeeded)
  # order of tiles is left to right, top to bottom, so
  xTileProgression <- rep(1:numXTiles, numYTiles)  
  yTileProgression <- rep(1:numYTiles, each = numXTiles)
  
  
  # make urls
  base_url <- 'http://tile.stamen.com/'
  base_url <- paste(base_url, maptype, '/', zoom, sep = '')
  urls <- paste(base_url, 
    apply(tilesNeeded, 1, paste, collapse = '/'), sep = '/')
  urls <- paste(urls, '.png', sep = '')
  if(urlonly) return(urls)

  # download and stich
  size <- 256 * c(length(xsNeeded), length(ysNeeded))  
  map <- matrix('NA', nrow = size[2], ncol = size[1])
  destfile <- paste(filename, 'png', sep = '.')
  
  for(k in seq_along(urls)){
    download.file(urls[[k]], destfile = destfile, quiet = !messaging)
    tile <- readPNG(destfile)    
    if(color == 'color'){
      tile <- apply(tile, 2, rgb)
    } else if(color == 'bw'){
      tile_dim <- dim(tile)
  	  tile <- gray(.30 * tile[,,1] + .59 * tile[,,2] + .11 * tile[,,3])
      dim(tile) <- tile_dim[1:2]
    }    
    tile <- tile

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
    n <- 2^zoom
    x <- vXY[1]; y <- vXY[2]
    c(
      left = x/n * 360 - 180,
      right = (x+1)/n * 360 - 180,
      top = atan(sinh( (1-2*(y+.5)/n)*pi )) * 180/pi,
      bottom = atan(sinh( (1-2*(y+1.5)/n)*pi )) * 180/pi
    )    
  }
  tileBboxes <- as.data.frame(t(apply(tilesNeeded, 1, bboxOfTile)))
  names(tileBboxes) <- c('left','right','top','bottom')
  mbbox <- c(
    left = min(tileBboxes$left),
    right = max(tileBboxes$right),
    bottom = min(tileBboxes$bottom),
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
    slat <- seq(mbbox['bottom'], mbbox['top'], length.out = size[2])    

    keep_x_ndcs <- which(bbox['left'] <= slon & slon <= bbox['right'])
    keep_y_ndcs <- which(bbox['bottom'] <= slat & slat <= bbox['top'])    
    
    map <- map[keep_y_ndcs, keep_x_ndcs]
  }
  
  # format map
  map <- as.raster(map)  
  class(map) <- c('ggmap','raster')
  attr(map, 'bb') <- data.frame(
    ll.lat = bbox['bottom'], ll.lon = bbox['left'],
    ur.lat = bbox['top'], ur.lon = bbox['right']
  )    

  
  # return
  map
}


















get_stamenmap_checkargs <- function(args){
  eargs <- lapply(args, eval)
  argsgiven <- names(args)

  with(eargs,{
  	
    # bbox arg
    if(!(is.numeric(bbox) && length(bbox) == 4)){
      stop('bounding box improperly specified.  see ?get_openstreetmap', call. = F)
    }
   
    # zoom arg
    if(!(is.numeric(zoom) && length(zoom) == 1 && 
    zoom == round(zoom) && zoom >= 0 && zoom <= 18)){
      stop('scale must be a postive integer 0-18, see ?get_stamenmap.', call. = F)
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
