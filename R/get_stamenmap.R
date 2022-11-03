#' Get a Stamen Map
#'
#' [get_stamenmap()] accesses a tile server for Stamen Maps and
#' downloads/stitches map tiles/formats a map image. Note that Stamen maps don't
#' cover the entire world.
#'
#' @param bbox a bounding box in the format c(lowerleftlon, lowerleftlat,
#'   upperrightlon, upperrightlat).
#' @param zoom a zoom level
#' @param maptype terrain, terrain-background, terrain-labels, terrain-lines,
#'   toner, toner-2010, toner-2011, toner-background, toner-hybrid,
#'   toner-labels, toner-lines, toner-lite, or watercolor.
#' @param crop crop raw map tiles to specified bounding box. if FALSE, the
#'   resulting map will more than cover the bounding box specified.
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param color color or black-and-white (use force = TRUE if you've already
#'   downloaded the images)
#' @param force if the map is on file, should a new map be looked up?
#' @param where where should the file drawer be located (without terminating
#'   "/")
#' @param https if TRUE, queries an https endpoint so that web traffic between
#'   you and the tile server is ecrypted using SSL.
#' @param ... ...
#' @return a ggmap object (a classed raster object with a bounding box
#'   attribute)
#' @seealso \url{http://maps.stamen.com/#watercolor}, [ggmap()]
#' @name get_stamenmap
#' @examples
#'
#' \dontrun{ some requires Google API key, see ?register_google; heavy network/time load
#'
#'
#' ## basic usage
#' ########################################
#'
#' bbox <- c(left = -97.1268, bottom = 31.536245, right = -97.099334, top = 31.559652)
#'
#' ggmap(get_stamenmap(bbox, zoom = 13))
#' ggmap(get_stamenmap(bbox, zoom = 14))
#' ggmap(get_stamenmap(bbox, zoom = 15))
#' ggmap(get_stamenmap(bbox, zoom = 17, messaging = TRUE))
#'
#' place <- "mount everest"
#' (google <- get_googlemap(place, zoom = 9))
#' ggmap(google)
#' bbox_everest <- c(left = 86.05, bottom = 27.21, right = 87.81, top = 28.76)
#' ggmap(get_stamenmap(bbox_everest, zoom = 9))
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
#' get_stamenmap(bbox, maptype = "terrain")            %>% ggmap()
#' get_stamenmap(bbox, maptype = "terrain-background") %>% ggmap()
#' get_stamenmap(bbox, maptype = "terrain-labels")     %>% ggmap()
#' get_stamenmap(bbox, maptype = "terrain-lines")      %>% ggmap()
#' get_stamenmap(bbox, maptype = "toner")              %>% ggmap()
#' get_stamenmap(bbox, maptype = "toner-2010")         %>% ggmap()
#' get_stamenmap(bbox, maptype = "toner-2011")         %>% ggmap()
#' get_stamenmap(bbox, maptype = "toner-background")   %>% ggmap()
#' get_stamenmap(bbox, maptype = "toner-hybrid")       %>% ggmap()
#' get_stamenmap(bbox, maptype = "toner-labels")       %>% ggmap()
#' get_stamenmap(bbox, maptype = "toner-lines")        %>% ggmap()
#' get_stamenmap(bbox, maptype = "toner-lite")         %>% ggmap()
#' get_stamenmap(bbox, maptype = "watercolor")         %>% ggmap()
#'
#'
#' ## zoom levels
#' ########################################
#'
#' get_stamenmap(bbox, maptype = "watercolor", zoom = 11) %>% ggmap(extent = "device")
#' get_stamenmap(bbox, maptype = "watercolor", zoom = 12) %>% ggmap(extent = "device")
#' get_stamenmap(bbox, maptype = "watercolor", zoom = 13) %>% ggmap(extent = "device")
#' # get_stamenmap(bbox, maptype = "watercolor", zoom = 14) %>% ggmap(extent = "device")
#' # get_stamenmap(bbox, maptype = "watercolor", zoom = 15) %>% ggmap(extent = "device")
#' # get_stamenmap(bbox, maptype = "watercolor", zoom = 16) %>% ggmap(extent = "device")
#' # get_stamenmap(bbox, maptype = "watercolor", zoom = 17) %>% ggmap(extent = "device")
#' # get_stamenmap(bbox, maptype = "watercolor", zoom = 18) %>% ggmap(extent = "device")
#'
#'
#' ## https
#' ########################################
#'
#' bbox <- c(left = -97.1268, bottom = 31.536245, right = -97.099334, top = 31.559652)
#' get_stamenmap(bbox, zoom = 14, urlonly = TRUE)
#' get_stamenmap(bbox, zoom = 14, urlonly = TRUE, https = TRUE)
#' ggmap(get_stamenmap(bbox, zoom = 15, https = TRUE, messaging = TRUE))
#'
#'
#' ## more examples
#' ########################################
#'
#' gc <- geocode("rio de janeiro")
#'
#' get_stamenmap(bbox, zoom = 10) %>% ggmap() +
#'  geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#'
#' get_stamenmap(bbox, zoom = 10, crop = FALSE) %>% ggmap() +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#'
#' get_stamenmap(bbox, zoom = 10, maptype = "watercolor") %>% ggmap() +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#'
#' get_stamenmap(bbox, zoom = 10, maptype = "toner") %>% ggmap() +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)
#'
#'
#' # continental united states labels
#' c("left" = -125, "bottom" = 25.75, "right" = -67, "top" = 49) %>%
#'   get_stamenmap(zoom = 5, maptype = "toner-labels") %>%
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
#' qmap("the white house", zoom = 16, source = "stamen", maptype = "terrain")  +
#'   geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 3)
#'
#'
#'
#' ## known issues
#' ########################################
#'
#' # in some cases stamen's servers will not return a tile for a given map
#' # this tends to happen in high-zoom situations, but it is not always
#' # clear why it happens. these tiles will appear as blank parts of the map.
#'
#' # ggmap provides some tools to try to recover the missing tiles, but the
#' # servers seem pretty persistent at not providing the maps.
#'
#' bbox <- c(left = -97.1268, bottom = 31.536245, right = -97.099334, top = 31.559652)
#' ggmap(get_stamenmap(bbox, zoom = 17))
#' get_stamen_tile_download_fail_log()
#' retry_stamen_map_download()
#'
#'
#'
#'
#' }
#'
#'






#' @export
#' @rdname get_stamenmap
get_stamenmap <- function(
  bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344),
  zoom = 10, maptype = c("terrain","terrain-background","terrain-labels",
    "terrain-lines", "toner", "toner-2010", "toner-2011", "toner-background",
    "toner-hybrid", "toner-labels", "toner-lines", "toner-lite", "watercolor"),
  crop = TRUE, messaging = FALSE, urlonly = FALSE, color = c("color","bw"), force = FALSE,
  where = tempdir(), https = FALSE, ...
){

  # enumerate argument checking (added in lieu of checkargs function)
  args <- as.list(match.call(expand.dots = TRUE)[-1])
  argsgiven <- names(args)

  if ("location" %in% argsgiven) {
    cli::cli_warn("{.arg location} is not a valid argument to {.fn ggmap::get_stamenmap}; it is ignored.")
  }

  if("bbox" %in% argsgiven){
    if(!(is.numeric(bbox) && length(bbox) == 4)){
      cli::cli_abort("{.arg bbox} is improperly specified, see {.fn ggmap::get_openstreetmap}.")
    }
  }

  if("zoom" %in% argsgiven){
    if(!(is.numeric(zoom) && length(zoom) == 1 &&
    zoom == round(zoom) && zoom >= 0 && zoom <= 18)){
      cli::cli_abort("{.arg scale} must be a positive integer 0-18, see {.fn ggmap::get_stamenmap.}.")
    }
  }

  if("messaging" %in% argsgiven) stopifnot(is.logical(messaging))

  if("urlonly" %in% argsgiven) stopifnot(is.logical(urlonly))


  # color arg checked by match.arg


  # argument checking (no checks for language, region, markers, path, visible, style)
  #args <- as.list(match.call(expand.dots = TRUE)[-1])
  #if(checkargs) get_stamenmap_checkargs(args)
  maptype <- match.arg(maptype)
  color <- match.arg(color)
  if(is.null(names(bbox))) names(bbox) <- c("left","bottom","right","top")

  # set image type (stamen only)
  if(maptype %in% c("watercolor")){
    filetype <- "jpg"
    cli::cli_alert_info("Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.")
  } else {
    filetype <- "png"
    cli::cli_alert_info("Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.")
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
  ysNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, function(df) df$Y)))))
  tilesNeeded <- expand.grid(x = xsNeeded, y = ysNeeded)
  if(nrow(tilesNeeded) > 40){
    cli::cli_alert_info("{nrow(tilesNeeded)} tiles needed, this may take a while (try a smaller zoom?)")
  }


  # make urls - e.g. http://tile.stamen.com/[maptype]/[zoom]/[x]/[y].jpg
  base_url <- if (https) "https://stamen-tiles.a.ssl.fastly.net/" else "http://tile.stamen.com/"
  # base_url <- "http://b.b.tile.stamen.com/"
  base_url <- paste(base_url, maptype, "/", zoom, sep = "")
  urls <- paste(base_url, apply(tilesNeeded, 1, paste, collapse = "/"), sep = "/")
  urls <- paste(urls, filetype, sep = ".")
  if(messaging) cli::cli_alert_info("{length(urls)} tiles required.")
  if(urlonly) return(urls)


  # make list of tiles
  listOfTiles <- lapply(
    split(tilesNeeded, 1:nrow(tilesNeeded)),
    function(v) {
      v <- as.numeric(v)
      get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, messaging = messaging, https = https)
    }
  )


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
  attr(croppedmap, "source")  <- "stamen"
  attr(croppedmap, "maptype") <- maptype
  attr(croppedmap, "zoom")    <- zoom


  # return
  croppedmap
}
























get_stamenmap_tile <- function(maptype, zoom, x, y, color, force = FALSE, messaging = TRUE, where = tempdir(), https = FALSE, url){

  if (missing(url)) {

    # check arguments
    stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
    stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
    stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))


    # format url http://tile.stamen.com/[maptype]/[zoom]/[x]/[y].jpg
    if(maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
    domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
    url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")


    # lookup in archive
    tile <- file_drawer_get(url)
    if (!is.null(tile) && !force) return(tile)


    # message url
    if (messaging) source_url_msg(url)

  } else {

    url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
       maptype <- url_pieces[6]
          zoom <- url_pieces[7] %>% as.integer()
             x <- url_pieces[8] %>% as.integer()
             y <- url_pieces[9] %>% as.integer()
      filetype <- url_pieces[10]

  }


  # query server
  response <- httr::GET(url)


  # deal with bad responses
  if (response$status_code != 200L) {

    httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
    if (messaging) message("\n", appendLF = FALSE)
    log_stamen_tile_download_fail(url)
    tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)

  } else {

    # parse tile
    tile <- httr::content(response)
    tile <- aperm(tile, c(2, 1, 3))

    # convert to hex color
    if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", "terrain-labels", "terrain-lines")) {

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











log_stamen_tile_download_fail <- function(url) {

  if (exists("stamen_tile_download_fail_log", envir = ggmap_environment)) {

    assign(
      "stamen_tile_download_fail_log",
      unique(c(
        get("stamen_tile_download_fail_log", envir = ggmap_environment),
        url
      )),
      envir = ggmap_environment
    )

  } else {

    assign("stamen_tile_download_fail_log", url, envir = ggmap_environment)

  }

  invisible()

}








#' @export
#' @rdname get_stamenmap
get_stamen_tile_download_fail_log <- function() {

  if (!exists("stamen_tile_download_fail_log", envir = ggmap_environment)) {
    assign("stamen_tile_download_fail_log", character(0), envir = ggmap_environment)
  }

  get("stamen_tile_download_fail_log", envir = ggmap_environment)

}





#' @export
#' @rdname get_stamenmap
retry_stamen_map_download <- function() {

  if (!exists("stamen_tile_download_fail_log", envir = ggmap_environment)) {

    return(invisible())

  } else {

    get_stamen_tile_download_fail_log() %>%
      map(~ get_stamenmap_tile("url" = .x, "force" = TRUE))

  }

  invisible()

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
