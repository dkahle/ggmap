#' Get a Stamen Map
#'
#' \code{get_stamenmap} accesses a tile server for Stamen Maps and
#' downloads/stitches map tiles/formats a map image. Note that
#' Stamen maps don't cover the entire world, e.g.
#' \url{http://tile.stamen.com/terrain/#4/30.28/-87.21}
#'
#' @param bbox a bounding box in the format c(lowerleftlon,
#'   lowerleftlat, upperrightlon, upperrightlat).
#' @param zoom a zoom level
#' @param maptype terrain, terrain-background, terrain-labels,
#'   terrain-lines, toner, toner-2010, toner-2011, toner-background,
#'   toner-hybrid, toner-labels, toner-lines, toner-lite, or
#'   watercolor.
#' @param crop crop raw map tiles to specified bounding box
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param color color or black-and-white
#' @param force if the map is on file, should a new map be looked
#'   up?
#' @param where where should the file drawer be located (without
#'   terminating "/")
#' @param ... ...
#' @return a ggmap object (a classed raster object with a bounding
#'   box attribute)
#' @seealso \url{http://maps.stamen.com/#watercolor},
#'   \code{\link{ggmap}}
#' @export
#' @examples
#'
#' \dontrun{ # to diminish run check time
#'
#' gc <- geocode("baylor university")
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
#' # note that the osm code may not run due to overloaded
#' # servers.
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
#' ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 11), extent = "device")
#' ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 12), extent = "device")
#' ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 13), extent = "device")
#' ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 14), extent = "device")
#' # ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 15), extent = "device")
#' # ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 16), extent = "device")
#' # ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 17), extent = "device")
#' # ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 18), extent = "device")
#'
#' stamen <- get_stamenmap(bbox, zoom = 15)
#' ggmap(stamen) +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#'
#' stamen <- get_stamenmap(bbox, zoom = 15, crop = FALSE)
#' ggmap(stamen) +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#'
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
#' # here's an interesting example:
#' us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
#' map <- get_stamenmap(us, zoom = 5, maptype = "toner-labels")
#' ggmap(map)
#'
#'
#'
#' # accuracy check - white house
#' gc <- geocode("the white house")
#'
#' qmap("the white house", zoom = 16)  +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 3)
#'
#' qmap("the white house", zoom = 16, source = "stamen", maptype = "terrain")  +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 3)
#'
#'
#'
#'
#'
#' # accuracy check - statue of liberty
#' # see https://github.com/dkahle/ggmap/issues/32
#'
#' gc <- geocode("statue of liberty")
#'
#' googMapZ10 <- get_googlemap(center = as.numeric(gc))
#' bbZ10 <- attr(googMapZ10, "bb")
#' stamMapZ10 <- get_stamenmap(bb2bbox(bbZ10))
#'
#' ggmap(googMapZ10) +
#'   geom_point(
#'     aes(x = lon, y = lat),
#'     data = gc, colour = "red", size = 3
#'   )
#'
#' ggmap(stamMapZ10) +
#'   geom_point(
#'     aes(x = lon, y = lat),
#'     data = gc, colour = "red", size = 3
#' )
#'
#'
#' # using a higher zoom
#' googMapZ15 <- get_googlemap(center = as.numeric(gc), zoom = 15)
#' bbZ15 <- attr(googMapZ15, "bb")
#' stamMapZ15 <- get_stamenmap(bb2bbox(bbZ15),
#'   zoom = calc_zoom(bb2bbox(bbZ15))
#' )
#'
#' ggmap(googMapZ15) +
#'   geom_point(
#'     aes(x = lon, y = lat),
#'     data = gc, colour = "red", size = 3
#' )
#'
#' ggmap(stamMapZ15) +
#'   geom_point(
#'     aes(x = lon, y = lat),
#'     data = gc, colour = "red", size = 3
#'   )
#'
#'
#' # using a lower zoom
#' googMapZ5 <- get_googlemap(center = as.numeric(gc), zoom = 4)
#' bbZ5 <- attr(googMapZ5, "bb")
#' stamMapZ5 <- get_stamenmap(bb2bbox(bbZ5),
#'   zoom = calc_zoom(bb2bbox(bbZ5))
#' )
#'
#' ggmap(googMapZ5) +
#'   geom_point(
#'     aes(x = lon, y = lat),
#'     data = gc, colour = "red", size = 3
#'   )
#'
#' ggmap(stamMapZ5) +
#'   geom_point(
#'     aes(x = lon, y = lat),
#'     data = gc, colour = "red", size = 3
#'   )
#'
#'
#' stamMapZ5unCropped <- get_stamenmap(bb2bbox(bbZ5),
#'   zoom = calc_zoom(bb2bbox(bbZ5)),
#'   crop = FALSE)
#'
#' ggmap(stamMapZ5unCropped) +
#'   geom_point(
#'     aes(x = lon, y = lat),
#'     data = gc, colour = "red", size = 3
#'   )
#'
#' qmap(location = c(lon = -74.0445, lat = 40.68925),
#'     zoom = 16, source = "stamen")  +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 3)
#'
#' } # end dontrun
#'
get_stamenmap <- function(
  bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344),
  zoom = 10, maptype = c("terrain","terrain-background","terrain-labels",
    "terrain-lines", "toner", "toner-2010", "toner-2011", "toner-background",
    "toner-hybrid", "toner-labels", "toner-lines", "toner-lite", "watercolor"),
  crop = TRUE, messaging = FALSE, urlonly = FALSE, color = c("color","bw"), force = FALSE,
  where = tempdir(), ...
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

  # argument checking (no checks for language, region, markers, path, visible, style)
  #args <- as.list(match.call(expand.dots = TRUE)[-1])
  #if(checkargs) get_stamenmap_checkargs(args)
  maptype <- match.arg(maptype)
  color <- match.arg(color)
  if(is.null(names(bbox))) names(bbox) <- c("left","bottom","right","top")

  # set image type (stamen only)
  if(maptype %in% c("terrain","terrain-background","watercolor")){
    filetype <- "jpg"
  } else {
    filetype <- "png"
  }

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
  if(!crop) {
    # additional map meta-data
    attr(map, "source")  <- "stamen"
    attr(map, "maptype") <- maptype
    attr(map, "zoom")    <- zoom

    # return
    return(map)
  }


  # crop map
  if(crop){
  	mbbox <- attr(map, "bb")

  	size <- 256 * c(length(xsNeeded), length(ysNeeded))

    # slon is the sequence of lons corresponding to the pixels
    # left to right
  	slon <- seq(mbbox$ll.lon, mbbox$ur.lon, length.out = size[1])

    # slat is the sequence of lats corresponding to the pixels
    # bottom to top
    # slat is more complicated due to the mercator projection
    slat <- vector("double", length = 256*length(ysNeeded))
    for(k in seq_along(ysNeeded)){
      slat[(k-1)*256 + 1:256] <-
        sapply(as.list(0:255), function(y){
          XY2LonLat(X = xsNeeded[1], Y = ysNeeded[k], zoom, x = 0, y = y)$lat
        })
    }
    slat <- rev(slat)
    ##slat <- seq(mbbox$ll.lat, mbbox$ur.lat, length.out = size[2])

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

  # additional map meta-data
  attr(croppedmap, "source")  <- "stamen"
  attr(croppedmap, "maptype") <- maptype
  attr(croppedmap, "zoom")    <- zoom


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

















get_stamenmap_tile <- function(maptype, zoom, x, y, force = FALSE, messaging = TRUE, where = tempdir()){

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
  tile <- file_drawer_get(url)
  if (!is.null(tile) && !force) return(tile)

  # grab if not in archive
  tmp <- tempfile()
  download.file(url, destfile = tmp, quiet = !messaging, mode = "wb")
  if(TRUE) message(paste0("Map from URL : ", url))

  # read in
  if(maptype %in% c("terrain","terrain-background","watercolor")){
    tile <- readJPEG(tmp)
  } else {
    tile <- readPNG(tmp)
  }


  # convert to colors
  # toner-lines treated differently for alpha
  if(maptype %in% c("toner-hybrid", "toner-labels", "toner-lines",
                    "terrain-labels", "terrain-lines")){
    tile <- t(apply(tile, 1:2, function(x) rgb(x[1], x[2], x[3], x[4])))
  } else {
    tile <- t(apply(tile, 2, rgb))
  }


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

  # store
  file_drawer_set(url, tile)

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
