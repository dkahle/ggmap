#' Get an OpenStreetMap
#'
#' \code{get_openstreetmap} accesses a tile server for OpenStreetMap and
#' downloads/stitches map tiles/formats a map image.  If you don't know how to
#' get the map you want, go to \url{http://www.openstreetmap.org/}, navigate to
#' the map extent that you want, click the export tab at the top of the page,
#' and copy the information into this function.
#'
#' See \url{http://www.openstreetmap.org/copyright} for license and copyright
#' information.
#'
#' As a prerequisite for downloading OpenStreetMap tiles with this function,
#' persistent tile caching must be enabled by setting the
#' \code{"ggmap.file_drawer"} \link[=options]{option}.
#' See \code{\link{file_drawer}}.
#'
#' @param bbox a bounding box in the format c(lowerleftlon, lowerleftlat,
#'   upperrightlon, upperrightlat)
#' @param zoom a zoom level
#' @param crop crop raw map tiles to specified bounding box
#' @param format character string providing image format - png, jpeg, svg, pdf,
#'   and ps formats
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param color color or black-and-white
#' @param force if the map is on file, should a new map be looked
#'   up? Use responsibly.
#' @param ... ...
#' @return a ggmap object (a classed raster object with a bounding box attribute)
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \url{http://www.openstreetmap.org/}, \code{\link{ggmap}}
#' @export
#' @examples
#'
#' get_openstreetmap(urlonly = TRUE)
#' \dontrun{
#' # osm servers get overloaded, which can result in
#' # erroneous failed checks
#'
#' # "ggmap.file_drawer" option must be set
#' osm <- get_openstreetmap()
#' ggmap(osm)
#'
#' }
#'
get_openstreetmap <- function(
  bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344),
  zoom = 10, crop = TRUE, format = c('png', 'jpeg', 'svg', 'pdf', 'ps'), messaging = FALSE,
  urlonly = FALSE, color = c('color','bw'), force = FALSE, ...
){

  # enumerate argument checking (added in lieu of checkargs function)
  args <- as.list(match.call(expand.dots = TRUE)[-1])
  argsgiven <- names(args)

  # argument checking (no checks for language, region, markers, path, visible, style)
  #if(checkargs) get_openstreetmap_checkargs(args)

  if('bbox' %in% argsgiven){
    if(!(is.numeric(bbox) && length(bbox) == 4)){
      stop('bounding box improperly specified.  see ?get_openstreetmap', call. = F)
    }
  }

  if("zoom" %in% argsgiven){
    if(!(is.numeric(zoom) && length(zoom) == 1 &&
         zoom == round(zoom) && zoom >= 0 && zoom <= 16)){
      stop("zoom must be a positive integer 0-16, see ?get_openstreetmap.", call. = F)
    }
  }

  if('messaging' %in% argsgiven) stopifnot(is.logical(messaging))

  if('urlonly' %in% argsgiven) stopifnot(is.logical(urlonly))

  format <- match.arg(format)
  if(format != 'png') stop('currently only the png format is supported.', call. = F)

  # color arg checked by match.arg
  color <- match.arg(color)

  if('checkargs' %in% argsgiven){
    .Deprecated(msg = 'checkargs argument deprecated, args are always checked after v2.1.')
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
    message(paste0(nrow(tilesNeeded), " tiles needed, this may take a while ",
                   "(try a smaller zoom)."))
  }

  if(messaging) message(nrow(tilesNeeded), " tiles required.")
  if(urlonly) {
    # make urls
    urls <- sprintf("http://%s.tile.openstreetmap.org/",
                    sample(letters[1:3], nrow(tilesNeeded),
                           replace = TRUE))
    urls <- paste0(urls, zoom)
    urls <- paste(urls,
                  apply(tilesNeeded, 1, paste, collapse = "/"), sep = "/")
    urls <- paste0(urls, ".png")
    return(urls)
  }

  if (is.null(getOption("ggmap.file_drawer"))) {
    stop("option 'ggmap.file_drawer' must be set")
  }

  uagent <- ggmap_useragent()

  # make list of tiles
  listOfTiles <- lapply(split(tilesNeeded, 1:nrow(tilesNeeded)), function(v){
    v <- as.numeric(v)
    get_openstreetmap_tile(zoom, v[1], v[2], color, uagent, force = force,
                           messaging = messaging)
  })


  # stitch tiles together
  map <- stitch(listOfTiles)


  # format map and return if not cropping
  if(!crop) {
    # additional map meta-data
    attr(map, "source")  <- "osm"
    attr(map, "maptype") <- "openstreetmap"
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
  attr(croppedmap, "source")  <- "osm"
  attr(croppedmap, "maptype") <- "openstreetmap"
  attr(croppedmap, "zoom")    <- zoom


  # return
  croppedmap
}

get_openstreetmap_tile <- function(zoom, x, y, color, ua, force = FALSE,
                                   messaging = TRUE) {

  # check arguments
  is.wholenumber <- function (x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol

  stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:16))
  stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
  stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))

  # format url http://[abc].tile.openstreetmap.org/${z}/${x}/${y}.png
  url <- sprintf("http://a.tile.openstreetmap.org/%i/%i/%i.png", zoom, x, y)

  # lookup in archive
  tile <- file_drawer_get(url)
  if (!is.null(tile) && !force) {
    if (color == "color") {
      return(tile)
    } else {
      return(tile_to_bw(tile))
    }
  }

  # grab if not in archive
  url2 <- sub("//a", paste0("//", sample(letters[1:3], 1)), url, fixed = TRUE)
  tmp <- tempfile()
  downloaded <- suppressWarnings(try(
    curl_download(url2, destfile = tmp, quiet = !messaging, mode = "wb",
                  handle = new_handle(useragent = ua)), silent = TRUE
  ))

  # message url
  download_error <- inherits(downloaded, "try-error")
  if(download_error) {
    message(paste0("Source FAILED : ", url2))
  } else {
    message(paste0("Source : ", url2))
  }

  # read in/format tile
  if (download_error) {

    tile <- array(NA, dim = c(256L, 256L))

  } else {

    # read in
    tile <- readPNG(tmp)

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
  if(!download_error) file_drawer_set(url, tile)

  # return
  if (color == "color") {
    tile
  } else {
    tile_to_bw(tile)
  }
}
