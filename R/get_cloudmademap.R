#' Get a CloudMade map.
#'
#' \code{get_cloudmademap} accesses a tile server for Stamen Maps and
#' downloads/stitches map tiles/formats a map image. This function requires an
#' api key which can be obtained for free from http://cloudmade.com/user/show
#' (defunct?). Thousands of maptypes ("styles"), including create-your-own
#' options, are available from http://maps.cloudmade.com/editor (defunct).
#'
#' @param bbox a bounding box in the format c(lowerleftlon, lowerleftlat,
#'   upperrightlon, upperrightlat).
#' @param zoom a zoom level
#' @param api_key character string containing cloud made api key, see details
#' @param maptype an integer of what cloud made calls style, see details
#' @param highres double resolution
#' @param crop crop raw map tiles to specified bounding box
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param filename destination file for download (file extension added according
#'   to format). Default \code{NULL} means a random \code{\link{tempfile}}.
#' @param color color or black-and-white
#' @param ... ...
#' @return a ggmap object (a classed raster object with a bounding box
#'   attribute)
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso http://maps.cloudmade.com/ (defunct), \code{\link{ggmap}}
#' @export
#' @examples
#'
#'
#' #api_key <- '<your api key here>'
#' #api_key <- 'b23b0358e87c4ff99f81029eda25c903'
#'
#' #map <- get_cloudmademap(api_key = api_key)
#' #ggmap(map)
#'
#' #map <- get_cloudmademap(maptype = 997, api_key = api_key)
#' #ggmap(map)
#'
#'
get_cloudmademap <- function(
  bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344),
  zoom = 10, api_key, maptype = 1, highres = TRUE, crop = TRUE, messaging = FALSE,
  urlonly = FALSE, filename = NULL, color = c('color','bw'), ...
){

  .Defunct("CloudMade discontinued its static maps service.")

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
      stop('scale must be a positive integer 0-18, see ?get_stamenmap.', call. = F)
    }
  }

  if('maptype' %in% argsgiven){
    if(!(is.numeric(maptype) && length(maptype) == 1 &&
        maptype == round(maptype) && maptype > 0)){
      stop('maptype must be a positive integer, see ?get_cloudmademap.', call.=F)
    }
  }

  if('api_key' %in% argsgiven){
    if(!(is.character(api_key) && length(api_key) == 1)){
      stop('api_key improperly specified, see ?get_cloudmademap.', call.=F)
    }
  } else {
    stop('api_key must be specified, see ?get_cloudmademap.')
  }

  if('highres' %in% argsgiven) stopifnot(is.logical(highres))

  if('messaging' %in% argsgiven) stopifnot(is.logical(messaging))

  if('urlonly' %in% argsgiven) stopifnot(is.logical(urlonly))

  if(is.null(filename)){
    destfile <- tempfile(fileext = ".png")
  } else{
    filename_stop <- TRUE
    if(is.character(filename) && length(filename) == 1) filename_stop <- FALSE
    if(filename_stop) stop('improper filename specification, see ?get_googlemap.', call. = F)
    destfile <- paste(filename, 'png', sep = '.')
  }

  # color arg checked by match.arg

  if('checkargs' %in% argsgiven){
    .Deprecated(msg = 'checkargs argument deprecated, args are always checked after v2.1.')
  }


  # argument checking (no checks for language, region, markers, path, visible, style)
  #args <- as.list(match.call(expand.dots = TRUE)[-1])
  #if(checkargs) get_cloudmademap_checkargs(args)
  color <- match.arg(color)
  if(is.null(names(bbox))) names(bbox) <- c('left','bottom','right','top')
  if(highres) maptype <- paste(maptype, '@2x', sep = '')

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
  base_url <- 'http://b.tile.cloudmade.com/'
  base_url <- paste(base_url, api_key, '/', maptype, '/', 256, '/', zoom, sep = '')
  urls <- paste(base_url,
    apply(tilesNeeded, 1, paste, collapse = '/'), sep = '/')
  urls <- paste(urls, '.png', sep = '')
  if(messaging) message(length(urls), ' tiles required.')
  if(urlonly) return(urls)

  # download and stitch
  size <- 256 * c(length(xsNeeded), length(ysNeeded))
  map <- matrix('NA', nrow = size[2], ncol = size[1])

  for(k in seq_along(urls)){
    download.file(urls[[k]], destfile = destfile, quiet = !messaging, mode = 'wb')
    tile <- readPNG(destfile)
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

    # additional map meta-data
    attr(map, "source")  <- "google"
    attr(map, "maptype") <- maptype
    attr(map, "zoom")    <- zoom

    # return
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

  # additional map meta-data
  attr(croppedmap, "source")  <- "cloudmade"
  attr(croppedmap, "maptype") <- maptype
  attr(croppedmap, "zoom")    <- zoom


  # return
  croppedmap
}


















# get_cloudmademap_checkargs <- function(args){
#   eargs <- lapply(args, eval)
#   argsgiven <- names(args)
#
#   with(eargs,{
#
#     # bbox arg
#     if('bbox' %in% argsgiven){
#       if(!(is.numeric(bbox) && length(bbox) == 4)){
#         stop('bounding box improperly specified.  see ?get_openstreetmap', call. = F)
#       }
#     }
#
#     # zoom arg
#     if('zoom' %in% argsgiven){
#       if(!(is.numeric(zoom) && length(zoom) == 1 &&
#       zoom == round(zoom) && zoom >= 0 && zoom <= 18)){
#         stop('scale must be a positive integer 0-18, see ?get_stamenmap.', call. = F)
#       }
#     }
#
#     # maptype arg
#     if('maptype' %in% argsgiven){
#       if(!(is.numeric(maptype) && length(maptype) == 1 &&
#           maptype == round(maptype) && maptype > 0)){
#         stop('maptype must be a positive integer, see ?get_cloudmademap.', call.=F)
#       }
#     }
#
#     # api_key arg
#     if('api_key' %in% argsgiven){
#       if(!(is.character(api_key) && length(api_key) == 1)){
#         stop('api_key improperly specified, see ?get_cloudmademap.', call.=F)
#       }
#     } else {
#       stop('api_key must be specified, see ?get_cloudmademap.')
#     }
#
#     # highres arg
#     if('highres' %in% argsgiven){
#       stopifnot(is.logical(highres))
#     }
#
#     # messaging arg
#     if('messaging' %in% argsgiven){
#       stopifnot(is.logical(messaging))
#     }
#
#     # urlonly arg
#     if('urlonly' %in% argsgiven){
#       stopifnot(is.logical(urlonly))
#     }
#
#     # filename arg
#     if('filename' %in% argsgiven){
#       filename_stop <- TRUE
#       if(is.character(filename) && length(filename) == 1) style_stop <- FALSE
#       if(filename_stop) stop('improper filename specification, see ?get_googlemap.', call. = F)
#     }
#
#     # color arg checked by match.arg
#
#
#   }) # end with
# }
#
#
#
