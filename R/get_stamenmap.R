#' Get a Stamen Map
#'
#' get_stamenmap accesses a tile server for Stamen Maps and downloads/stiches map tiles/formats a map image.
#'
#' Note that Stamen maps don't cover the entire world.  For example, see http://tile.stamen.com/terrain/#4/30.28/-87.21
#' 
#' @param bbox a bounding box in the format c(lowerleftlon, lowerleftlat, upperrightlon, upperrightlat).
#' @param zoom a zoom level
#' @param maptype terrain, terrain-background, terrain-labels, terrain-lines, toner, toner-2010, toner-2011, toner-background, toner-hybrid, toner-labels, toner-lines, toner-lite, or watercolor
#' @param crop crop raw map tiles to specified bounding box
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param color color or black-and-white
#' @param force if the map is on file, should a new map be looked up?
#' @param ... ...
#' @details accesses stamen maps.
#' @return a ggmap object (a classed raster object with a bounding box attribute)
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \url{http://maps.stamen.com/#watercolor}, \code{\link{ggmap}}
#' @export
#' @examples
#'
#' 
#' \dontrun{ 
#'
#' gc <- geocode("marrs mclean science building, baylor university")
#' google <- get_googlemap("baylor university", zoom = 15)
#' ggmap(google) +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#' 
#' bbox <- c(left = -97.132, bottom = 31.536, right = -97.105, top = 31.560)
#' ggmap(get_stamenmap(bbox, zoom = 13))
#' ggmap(get_stamenmap(bbox, zoom = 14))
#' ggmap(get_stamenmap(bbox, zoom = 15))
#' # ggmap(get_stamenmap(bbox, zoom = 16))
#' # ggmap(get_stamenmap(bbox, zoom = 17))
#' 
#' 
#' # various maptypes are available.  bump it up to zoom = 15 for better resolution.
#' ggmap(get_stamenmap(bbox, maptype = "terrain", zoom = 14))
#' ggmap(get_stamenmap(bbox, maptype = "terrain-background", zoom = 14))
#' ggmap(get_stamenmap(bbox, maptype = "terrain-labels", zoom = 14)) 
#' ggmap(get_stamenmap(bbox, maptype = "terrain-lines", zoom = 14)) 
#' ggmap(get_stamenmap(bbox, maptype = "toner", zoom = 14)) 
#' ggmap(get_stamenmap(bbox, maptype = "toner-2010", zoom = 14))
#' ggmap(get_stamenmap(bbox, maptype = "toner-2011", zoom = 14))
#' ggmap(get_stamenmap(bbox, maptype = "toner-background", zoom = 14))
#' ggmap(get_stamenmap(bbox, maptype = "toner-hybrid", zoom = 14))
#' ggmap(get_stamenmap(bbox, maptype = "toner-labels", zoom = 14))
#' ggmap(get_stamenmap(bbox, maptype = "toner-lines", zoom = 14))
#' ggmap(get_stamenmap(bbox, maptype = "toner-lite", zoom = 14))
#' ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 14)) 
#' 
#' 
#' ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 11), extent = "device")
#' ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 12), extent = "device")
#' ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 13), extent = "device")
#' ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 14), extent = "device")
#' ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 15), extent = "device")
#' # ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 16), extent = "device")
#' # ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 17), extent = "device")
#' # ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 18), extent = "device")
#'
#' ggmap(get_stamenmap(bbox, maptype = "terrain-background", zoom = 14), extent = "device")
#' 
#' stamen <- get_stamenmap(bbox, zoom = 15)
#' ggmap(stamen) +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#' 
#' stamen <- get_stamenmap(bbox, zoom = 15, crop = FALSE)
#' ggmap(stamen) +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#' 
#' osm <- get_openstreetmap(bbox, scale = OSM_scale_lookup(15))
#' ggmap(osm) +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#' 
#' 
#' ggmap(get_stamenmap(bbox, zoom = 15, maptype = "watercolor"))+
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#' 
#' ggmap(get_stamenmap(bbox, zoom = 15, maptype = "toner"))+
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#' 
#'
#' # accuracy check
#' gc <- geocode("the white house")
#' 
#' qmap("the white house", zoom = 16)  +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 3)  
#'   
#' qmap("the white house", zoom = 16, source = "stamen", maptype == "terrain")  +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 3)    
#' 
#' }
#' 
get_stamenmap <- function(
  bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344), 
  zoom = 10, maptype = c("terrain","terrain-background","terrain-labels",
    "terrain-lines", "toner", "toner-2010", "toner-2011", "toner-background", 
    "toner-hybrid", "toner-labels", "toner-lines", "toner-lite", "watercolor"),   
  crop = TRUE, messaging = FALSE, 
  urlonly = FALSE, color = c("color","bw"), force = FALSE, ...
){
	
  # enumerate argument checking (added in lieu of checkargs function)	
  args <- as.list(match.call(expand.dots = TRUE)[-1])  
  argsgiven <- names(args)
 
  if("bbox" %in% argsgiven){
    if(!(is.numeric(bbox) && length(bbox) == 4)){
      stop("bounding box improperly specified.  see ?get_openstreetmap", call. = F)
    }
  }
   
  if("zoom" %in% argsgiven){    
    if(!(is.numeric(zoom) && length(zoom) == 1 && 
    zoom == round(zoom) && zoom >= 0 && zoom <= 18)){
      stop("scale must be a postive integer 0-18, see ?get_stamenmap.", call. = F)
    }    
  }
    
  if("messaging" %in% argsgiven) stopifnot(is.logical(messaging))

  if("urlonly" %in% argsgiven) stopifnot(is.logical(urlonly))     
    
    
  # color arg checked by match.arg  
    
  if("checkargs" %in% argsgiven){
    .Deprecated(msg = "checkargs argument deprecated, args are always checked after v2.1.")
  }      

  # set image type (stamen only)  	
  if(maptype %in% c("terrain","terrain-background","watercolor")){
    filetype <- "jpg"
  } else {
    filetype <- "png"
  }
  
  # argument checking (no checks for language, region, markers, path, visible, style)
  #args <- as.list(match.call(expand.dots = TRUE)[-1])  
  #if(checkargs) get_stamenmap_checkargs(args)
  maptype <- match.arg(maptype)    
  color <- match.arg(color)  
  if(is.null(names(bbox))) names(bbox) <- c("left","bottom","right","top")


  # determine tiles to get
  fourCorners <- expand.grid(
    lon = c(bbox["left"], bbox["right"]), 
    lat = c(bbox["bottom"], bbox["top"])
  )
  fourCorners$zoom <- zoom
  row.names(fourCorners) <- c("lowerleft","lowerright","upperleft","upperright")  
  fourCornersTiles <- apply(fourCorners, 1, function(v) LonLat2XY(v[1],v[2],v[3]))  

  xsNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, function(df) df$X)))))
  numXTiles <- length(xsNeeded)
  ysNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, function(df) df$Y)))))
  numYTiles <- length(ysNeeded)  
  tilesNeeded <- expand.grid(x = xsNeeded, y = ysNeeded)
  if(nrow(tilesNeeded) > 40){
    message(paste0(nrow(tilesNeeded), " tiles needed, this may take a while ",
      "(try a smaller zoom)."))
  }
  

  # make urls - e.g. http://tile.stamen.com/[maptype]/[zoom]/[x]/[y].jpg
  base_url <- "http://tile.stamen.com/"
  base_url <- paste(base_url, maptype, "/", zoom, sep = "")
  urls <- paste(base_url, 
    apply(tilesNeeded, 1, paste, collapse = "/"), sep = "/")
  urls <- paste(urls, filetype, sep = ".")
  if(messaging) message(length(urls), " tiles required.")
  if(urlonly) return(urls)  
  if(any(sapply(as.list(urls), url_lookup) != FALSE)) message("Using archived tiles...")


  # make list of tiles
  count <- 0
  nTiles <- nrow(tilesNeeded)
  listOfTiles <- lapply(split(tilesNeeded, 1:nrow(tilesNeeded)), function(v){
    v <- as.numeric(v)
    get_stamenmap_tile(maptype, zoom, v[1], v[2], force = force, messaging = messaging)
  })
  

  # stitch tiles together
  map <- stitch(listOfTiles)

  
  # format map and return if not cropping
  if(!crop) return(map)


  # crop map  
  if(crop){
  	mbbox <- attr(map, "bb")
  	
  	size <- 256 * c(length(xsNeeded), length(ysNeeded))  
    slon <- seq(mbbox$ll.lon, mbbox$ur.lon, length.out = size[1])
    slat <- seq(mbbox$ll.lat, mbbox$ur.lat, length.out = size[2])    

    keep_x_ndcs <- which(bbox["left"] <= slon & slon <= bbox["right"])
    keep_y_ndcs <- sort( size[2] - which(bbox["bottom"] <= slat & slat <= bbox["top"]) )
    
    croppedmap <- map[keep_y_ndcs, keep_x_ndcs]
  }
  

  # format map
  croppedmap <- as.raster(croppedmap)  
  class(croppedmap) <- c("ggmap","raster")
  attr(croppedmap, "bb") <- data.frame(
    ll.lat = bbox["bottom"], ll.lon = bbox["left"],
    ur.lat = bbox["top"], ur.lon = bbox["right"]
  )    
  
  
  # return
  croppedmap
}


















get_stamenmap_checkargs <- function(args){
  eargs <- lapply(args, eval)
  argsgiven <- names(args)

  with(eargs,{
  	
    # bbox arg
    if("bbox" %in% argsgiven){
      if(!(is.numeric(bbox) && length(bbox) == 4)){
        stop("bounding box improperly specified.  see ?get_openstreetmap", call. = F)
      }
    }
   
    # zoom arg
    if("zoom" %in% argsgiven){    
      if(!(is.numeric(zoom) && length(zoom) == 1 && 
      zoom == round(zoom) && zoom >= 0 && zoom <= 18)){
        stop("scale must be a postive integer 0-18, see ?get_stamenmap.", call. = F)
      }    
    }
    
    # messaging arg
    if("messaging" %in% argsgiven){    
      stopifnot(is.logical(messaging))      
    }
    
    # urlonly arg
    if("urlonly" %in% argsgiven){    
      stopifnot(is.logical(urlonly))      
    }    
    
    
    # color arg checked by match.arg    
        
  
  }) # end with
}

















get_stamenmap_tile <- function(maptype, zoom, x, y, force = FALSE, messaging = TRUE){

  # check arguments
  is.wholenumber <- 
    function (x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol
		
  stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
  stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
  stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
  
  # format url http://tile.stamen.com/[maptype]/[zoom]/[x]/[y].jpg
  if(maptype %in% c("terrain","terrain-background","watercolor")){
    filetype <- "jpg"
  } else {
    filetype <- "png"
  }
  url <- paste0(paste0(c("http://tile.stamen.com", maptype, zoom, x, y), collapse = "/"), ".", filetype)
  
  # lookup in archive
  lookup <- url_lookup(url)
  if(lookup != FALSE && force == FALSE) return(recall_ggmap(url))
  
  # grab if not in archive
  download.file(url, destfile = paste0("ggmapFileDrawer/ggmapTemp.", filetype),
    quiet = !messaging, mode = "wb")
  if(TRUE) message(paste0("Map from URL : ", url))  
  
  # read in and format
  if(maptype %in% c("terrain","terrain-background","watercolor")){
    tile <- readJPEG("ggmapFileDrawer/ggmapTemp.jpg")
  } else {
    tile <- readPNG("ggmapFileDrawer/ggmapTemp.png")
  }
  tile <- t(apply(tile, 2, rgb))
  
  # determine bbox of map. note : not the same as the argument bounding box -
  # the map is only a covering of the bounding box extent the idea is to get
  # the lower left tile and the upper right tile and compute their bounding boxes
  # tiles are referenced by top left of tile, starting at 0,0
  # see http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  lonlat_upperleft <- XY2LonLat(x, y, zoom)  
  lonlat_lowerright <- XY2LonLat(x, y, zoom, 255, 255)
  bbox <- c(
    left = lonlat_upperleft$lon,
    bottom = lonlat_lowerright$lat,
    right = lonlat_lowerright$lon,
    top = lonlat_upperleft$lat
  )
  bb <- data.frame(
    ll.lat = unname(bbox["bottom"]),
    ll.lon = unname(bbox["left"]),
    ur.lat = unname(bbox["top"]),
    ur.lon = unname(bbox["right"])
  )

  # format
  class(tile) <- c("ggmap", "raster")
  attr(tile, "bb") <- bb
  
  # archive
  archive_ggmap(tile, url, 
    file = paste0(paste0(c(maptype, zoom, x, y), collapse = "-"), ".rds")
  )
  
  # return
  tile
}













stitch <- function(tiles){
	
  # trick R CMD check
  ll.lat <- NULL; rm(ll.lat);
  ll.lon <- NULL; rm(ll.lon);  
    
  # determine bounding box
  bbs <- ldply(tiles, function(x) attr(x, "bb"))    

  bigbb <- data.frame(
    ll.lat = min(bbs$ll.lat),
    ll.lon = min(bbs$ll.lon),
    ur.lat = max(bbs$ur.lat),
    ur.lon = max(bbs$ur.lon)
  )
  
  # determine positions of tile in slate (aggregate)  
  order <- as.numeric( arrange(bbs, desc(ll.lat), ll.lon)$.id )
  tiles <- tiles[order]
  tiles <- lapply(tiles, as.matrix) # essential for cbind/rbind to work properly!

  # split tiles, then squeeze together from top and bottom
  # and then squeeze together from left and right
  nrows <- length( unique(bbs$ll.lat) )
  ncols <- length( unique(bbs$ll.lon) )    
  tiles <- split(tiles, rep(1:nrows, each = ncols))
  tiles <- lapply(tiles, function(x) Reduce(cbind, x))
  tiles <- Reduce(rbind, tiles)
  
  tiles <- as.raster(tiles)
  class(tiles) <- c("ggmap", "raster")
  attr(tiles, "bb") <- bigbb

  tiles
}
