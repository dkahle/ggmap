# NOTE: Be sure to update the documentation for maptype both in this file and
# in get_map.R whenever you change this!
# TODO: Extend with our own styles!
STADIA_BASEMAP_TYPES <- c("stamen_terrain", "stamen_toner", "stamen_toner_lite",
                          "stamen_watercolor", "alidade_smooth", "alidade_smooth_dark",
                          "outdoors")
STADIA_BACKGROUND_LAYERGROUP_TYPES <- c("stamen_terrain_background", "stamen_toner_background")
STADIA_TRANSPARENT_LAYERGROUP_TYPES <- c("stamen_terrain_labels", "stamen_terrain_lines",
                                         "stamen_toner_labels", "stamen_toner_lines")
STADIA_VALID_MAP_TYPES <- c(STADIA_BASEMAP_TYPES,
                            STADIA_BACKGROUND_LAYERGROUP_TYPES,
                            STADIA_TRANSPARENT_LAYERGROUP_TYPES)

#' Get a Map from Stadia Maps
#'
#' [get_stadiamap()] accesses a tile server for Stadia Maps and
#' downloads/stitches map tiles/formats a map image.
#'
#' @param bbox a bounding box in the format c(lowerleftlon, lowerleftlat,
#'   upperrightlon, upperrightlat).
#' @param zoom a zoom level
#' @param maptype stamen_terrain, stamen_toner, stamen_toner_lite, stamen_watercolor,
#'   stamen_terrain_background, stamen_toner_background,
#'   stamen_terrain_lines, stamen_terrain_labels,
#'   stamen_toner_lines, stamen_toner_labels.
#' @param crop crop raw map tiles to specified bounding box. if FALSE, the
#'   resulting map will more than cover the bounding box specified.
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param color color or black-and-white (use force = TRUE if you've already
#'   downloaded the images)
#' @param force if the map is on file, should a new map be looked up?
#' @param where where should the file drawer be located (without terminating
#'   "/")
#' @param ... ...
#' @return a ggmap object (a classed raster object with a bounding box
#'   attribute)
#' @seealso \url{https://docs.stadiamaps.com/themes/}, [ggmap()]
#' @name get_stadiamap
#' @examples
#'
#' \dontrun{ requires a Stadia Maps API key. see ?register_stadiamaps
#'
#'
#' ## basic usage
#' ########################################
#'
#' bbox <- c(left = -97.1268, bottom = 31.536245, right = -97.099334, top = 31.559652)
#'
#' ggmap(get_stadiamap(bbox, zoom = 13))
#' ggmap(get_stadiamap(bbox, zoom = 14))
#' ggmap(get_stadiamap(bbox, zoom = 15))
#' ggmap(get_stadiamap(bbox, zoom = 17, messaging = TRUE))
#'
#' place <- "mount everest"
#' (google <- get_googlemap(place, zoom = 9))
#' ggmap(google)
#' bbox_everest <- c(left = 86.05, bottom = 27.21, right = 87.81, top = 28.76)
#' ggmap(get_stadiamap(bbox_everest, zoom = 9))
#'
#'
#'
#' ## map types
#' ########################################
#'
#' place <- "rio de janeiro"
#' google <- get_googlemap(place, zoom = 10)
#' ggmap(google)
#'
#' bbox <- bb2bbox(attr(google, "bb"))
#'
#' get_stadiamap(bbox, maptype = "stamen_terrain")            %>% ggmap()
#' get_stadiamap(bbox, maptype = "stamen_terrain_background") %>% ggmap()
#' get_stadiamap(bbox, maptype = "stamen_terrain_labels")     %>% ggmap()
#' get_stadiamap(bbox, maptype = "stamen_terrain_lines")      %>% ggmap()
#' get_stadiamap(bbox, maptype = "stamen_toner")              %>% ggmap()
#' get_stadiamap(bbox, maptype = "stamen_toner_background")   %>% ggmap()
#' get_stadiamap(bbox, maptype = "stamen_toner_labels")       %>% ggmap()
#' get_stadiamap(bbox, maptype = "stamen_toner_lines")        %>% ggmap()
#' get_stadiamap(bbox, maptype = "stamen_toner_lite")         %>% ggmap()
#' get_stadiamap(bbox, maptype = "stamen_watercolor")         %>% ggmap()
#'
#'
#' ## zoom levels
#' ########################################
#'
#' get_stadiamap(bbox, maptype = "stamen_watercolor", zoom = 11) %>% ggmap(extent = "device")
#' get_stadiamap(bbox, maptype = "stamen_watercolor", zoom = 12) %>% ggmap(extent = "device")
#' get_stadiamap(bbox, maptype = "stamen_watercolor", zoom = 13) %>% ggmap(extent = "device")
#' # get_stadiamap(bbox, maptype = "stamen_watercolor", zoom = 14) %>% ggmap(extent = "device")
#' # get_stadiamap(bbox, maptype = "stamen_watercolor", zoom = 15) %>% ggmap(extent = "device")
#' # get_stadiamap(bbox, maptype = "stamen_watercolor", zoom = 16) %>% ggmap(extent = "device")
#' # get_stadiamap(bbox, maptype = "stamen_watercolor", zoom = 17) %>% ggmap(extent = "device")
#' # get_stadiamap(bbox, maptype = "stamen_watercolor", zoom = 18) %>% ggmap(extent = "device")
#'
#' ## more examples
#' ########################################
#'
#' gc <- geocode("rio de janeiro")
#'
#' get_stadiamap(bbox, zoom = 10) %>% ggmap() +
#'  geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#'
#' get_stadiamap(bbox, zoom = 10, crop = FALSE) %>% ggmap() +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#'
#' get_stadiamap(bbox, zoom = 10, maptype = "stamen_watercolor") %>% ggmap() +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#'
#' get_stadiamap(bbox, zoom = 10, maptype = "stamen_toner") %>% ggmap() +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#'
#'
#' # continental united states labels
#' c("left" = -125, "bottom" = 25.75, "right" = -67, "top" = 49) %>%
#'   get_stadiamap(zoom = 5, maptype = "stamen_toner_labels") %>%
#'   ggmap()
#'
#'
#'
#'
#' # accuracy check - white house
#' gc <- geocode("the white house")
#'
#' qmap("the white house", zoom = 16)  +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 3)
#'
#' qmap("the white house", zoom = 16, source = "stadia", maptype = "stamen_terrain")  +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 3)
#'
#'
#'
#' ## known issues
#' ########################################
#'
#' # Stamen's original tilesets were raster renders built up over time, but have not been
#' # actively rendered for several years. As a consequence, some tiles simply do not exist,
#' # particularly at high zoom levels.
#' #
#' # The newer styles have been redesigned and are now generated live by Stadia Maps, so
#' # these are complete, but at the time of this writing, the Watercolor style is still incomplete.
#' }


#' @export
#' @rdname get_stadiamap
get_stadiamap <- function(
  bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344),
  zoom = 10, maptype = STADIA_VALID_MAP_TYPES,
  crop = TRUE, messaging = FALSE, urlonly = FALSE, color = c("color","bw"), force = FALSE,
  where = tempdir(), ...
){
  # enumerate argument checking (added in lieu of checkargs function)
  args <- as.list(match.call(expand.dots = TRUE)[-1])
  argsgiven <- names(args)

  if ("location" %in% argsgiven) {
    cli::cli_warn("{.arg location} is not a valid argument to {.fn ggmap::get_stadiamap}; it is ignored.")
  }

  if("bbox" %in% argsgiven){
    if(!(is.numeric(bbox) && length(bbox) == 4)){
      cli::cli_abort("{.arg bbox} is improperly specified, see {.fn ggmap::get_openstreetmap}.")
    }
  }

  if("zoom" %in% argsgiven){
    if(!(is.numeric(zoom) && length(zoom) == 1 &&
    zoom == round(zoom) && zoom >= 0 && zoom <= 18)){
      cli::cli_abort("{.arg scale} must be a positive integer 0-18, see {.fn ggmap::get_stadiamap.}.")
    }
  }

  if("messaging" %in% argsgiven) stopifnot(is.logical(messaging))

  if("urlonly" %in% argsgiven) stopifnot(is.logical(urlonly))


  # color arg checked by match.arg


  # argument checking (no checks for language, region, markers, path, visible, style)
  maptype <- match.arg(maptype)
  color <- match.arg(color)
  if(is.null(names(bbox))) names(bbox) <- c("left","bottom","right","top")

  # set image type (watercolor is JPEG-only)
  if(maptype %in% c("stamen_watercolor")){
    filetype <- "jpg"
  } else {
    filetype <- "png"
  }

  cli::cli_alert_info("\u00a9 Stadia Maps \u00a9 Stamen Design \u00a9 OpenMapTiles \u00a9 OpenStreetMap contributors.")

  # determine tiles to get
  fourCorners <- expand.grid(
    lon = c(bbox["left"], bbox["right"]),
    lat = c(bbox["bottom"], bbox["top"])
  )
  fourCorners$zoom <- zoom
  row.names(fourCorners) <- c("lowerleft","lowerright","upperleft","upperright")
  fourCornersTiles <- apply(fourCorners, 1, function(v) LonLat2XY(v[1],v[2],v[3]))

  xsNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, function(df) df$X)))))
  ysNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, function(df) df$Y)))))
  tilesNeeded <- expand.grid(x = xsNeeded, y = ysNeeded)
  if(nrow(tilesNeeded) > 40){
    cli::cli_alert_info("{nrow(tilesNeeded)} tiles needed, this may take a while (try a smaller zoom?)")
  }

  # Build up a list of URLs
  urls <- apply(tilesNeeded, 1, function(row) {
    get_stamen_url(maptype = maptype, zoom = zoom, x = row['x'], y = row['y'])
  })
  if(messaging) cli::cli_alert_info("{length(urls)} tiles required.")
  if(urlonly) return(urls)


  # make list of tiles
  listOfTiles <- lapply(
    split(tilesNeeded, 1:nrow(tilesNeeded)),
    function(v) {
      v <- as.numeric(v)
      get_stadiamap_tile(maptype, zoom, v[1], v[2], color, force = force, messaging = messaging)
    }
  )


  # stitch tiles together
  map <- stitch(listOfTiles)


  # format map and return if not cropping
  if(!crop) {
    # additional map meta-data
    attr(map, "source")  <- "stadia"
    attr(map, "maptype") <- maptype
    attr(map, "zoom")    <- zoom

    # return
    return(map)
  }


  # crop map
  if(crop){
  	mbbox <- attr(map, "bb")

  	size <- 256L * c(length(xsNeeded), length(ysNeeded))

    # slon is the sequence of lons corresponding to the pixels left to right
  	slon <- seq(mbbox$ll.lon, mbbox$ur.lon, length.out = size[1])

    # slat is the sequence of lats corresponding to the pixels bottom to top
    # slat is more complicated due to the mercator projection
    slat <- vector("double", length = 256L*length(ysNeeded))
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
  attr(croppedmap, "source")  <- "stadia"
  attr(croppedmap, "maptype") <- maptype
  attr(croppedmap, "zoom")    <- zoom


  # return
  croppedmap
}


get_stamen_url <- function(maptype, zoom, x, y) {
  # check arguments
  stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
  stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
  stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))

  if (!has_stadiamaps_key()) {
    cli::cli_abort("Stadia Maps requires an API key; see {.fn ggmap::register_stadiamaps}.")
  }

  key <- stadiamaps_key()

  # format URL
  if(maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
  url <- glue("https://tiles.stadiamaps.com/tiles/{maptype}/{zoom}/{x}/{y}.{filetype}?api_key={key}")

  return(url)
}



get_stadiamap_tile <- function(maptype, zoom, x, y, color, force = FALSE, messaging = TRUE, where = tempdir()){
  url <- get_stamen_url(maptype, zoom, x, y)

  # lookup in archive
  tile <- file_drawer_get(url)
  if (!is.null(tile) && !force) return(tile)

  # message url
  if (messaging) source_url_msg(url)


  # query server
  response <- httr::GET(url)


  # deal with bad responses
  if (response$status_code != 200L) {

    httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
    if (messaging) message("\n", appendLF = FALSE)
    tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)

  } else {

    # parse tile
    tile <- httr::content(response)
    tile <- aperm(tile, c(2, 1, 3))

    # convert to hex color (transparent layers)
    if (maptype %in% STADIA_TRANSPARENT_LAYERGROUP_TYPES) {

      if(color == "color") {
        tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], x[3], x[4]))
      } else {  # color == "bw" (all these are black and white naturally)
        tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], x[3], x[4]))
      }

    } else {

      if(color == "color") {
        tile <- apply(tile, 2, rgb)
      } else {  # color == "bw"
        tiled <- dim(tile)
        tile <- gray(.30 * tile[,,1] + .59 * tile[,,2] + .11 * tile[,,3])
        dim(tile) <- tiled[1:2]
      }

    }

  }




  # determine bbox of map. note : not the same as the argument bounding box -
  # the map is only a covering of the bounding box extent the idea is to get
  # the lower left tile and the upper right tile and compute their bounding boxes
  # tiles are referenced by top left of tile, starting at 0,0
  # see https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames

  lonlat_upperleft <- XY2LonLat(x, y, zoom)
  lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)

  bbox <- c(
    "left" = lonlat_upperleft$lon,
    "bottom" = lonlat_lowerright$lat,
    "right" = lonlat_lowerright$lon,
    "top" = lonlat_upperleft$lat
  )

  bb <- tibble(
    "ll.lat" = unname(bbox["bottom"]),
    "ll.lon" = unname(bbox["left"]),
    "ur.lat" = unname(bbox["top"]),
    "ur.lon" = unname(bbox["right"])
  )


  # format
  class(tile) <- c("ggmap", "raster")
  attr(tile, "bb") <- bb


  # cache
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
