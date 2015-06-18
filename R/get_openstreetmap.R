#' Get an OpenStreetMap
#'
#' \code{get_openstreetmap} accesses a tile server for OpenStreetMap and
#' downloads/formats a map image. This is simply a wrapper for the web-based
#' version at \url{http://www.openstreetmap.org/}.  If you don't know how to
#' get the map you want, go there, navigate to the map extent that you want,
#' click the export tab at the top of the page, and copy the information into
#' this function.
#'
#  In some cases the OSM server is unavailable,  in these cases you will
#' receive an error message from \code{download.file} with the message HTTP status
#' '503 Service Unavailable'.  You can confirm this by setting urlonly = TRUE,
#' and then entering the URL in a web browser.  the solution is either (1)
#' change sources or (2) wait for the OSM servers to come back up.
#'
#' See \url{http://www.openstreetmap.org/copyright} for license and copyright
#' information.
#'
#' @param bbox a bounding box in the format c(lowerleftlon, lowerleftlat,
#'   upperrightlon, upperrightlat)
#' @param scale scale parameter, see
#'   \url{http://wiki.openstreetmap.org/wiki/MinScaleDenominator}.  smaller
#'   scales provide a finer degree of detail, where larger scales produce more
#'   coarse detail.
#'
#'   The scale argument is a tricky number to correctly specify. In most cases,
#'   if you get an error when downloading an openstreetmap the error is
#'   attributable to an improper scale specification.
#'   \code{\link{OSM_scale_lookup}} can help; but the best way to get in the
#'   correct range is to go to \url{http://www.openstreetmap.org/}, navigate to
#'   the map of interest, click export at the top of the page, click 'map image'
#'   and then copy down the scale listed.
#' @param format character string providing image format - png, jpeg, svg, pdf,
#'   and ps formats
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param filename destination file for download (file extension added according
#'   to format)
#' @param color color or black-and-white
#' @param ... ...
#' @return a ggmap object (a classed raster object with a bounding box attribute)
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \url{http://www.openstreetmap.org/}, \code{\link{ggmap}}
#' @export
#' @examples
#'
#' \dontrun{
#' # osm servers get overloaded, which can result in
#' # erroneous failed checks
#'
#' osm <- get_openstreetmap(urlonly = TRUE)
#' ggmap(osm)
#'
#' }
get_openstreetmap <- function(
  bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344),
  scale = 606250, format = c('png', 'jpeg', 'svg', 'pdf', 'ps'), messaging = FALSE,
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

  if('scale' %in% argsgiven){
    if(!(is.numeric(scale) && length(scale) == 1 &&
    scale == round(scale) && scale > 0)){
      stop('scale must be a postive integer.', call. = F)
    }
  }

  if('messaging' %in% argsgiven) stopifnot(is.logical(messaging))

  if('urlonly' %in% argsgiven) stopifnot(is.logical(urlonly))

  if('filename' %in% argsgiven){
    filename_stop <- TRUE
    if(is.character(filename) && length(filename) == 1) filename_stop <- FALSE
    if(filename_stop) stop('improper filename specification, see ?get_openstreetmap.', call. = F)
  }

  # color arg checked by match.arg

  if('checkargs' %in% argsgiven){
    .Deprecated(msg = 'checkargs argument deprecated, args are always checked after v2.1.')
  }


  # argument checking (no checks for language, region, markers, path, visible, style)
  #args <- as.list(match.call(expand.dots = TRUE)[-1])
  #if(checkargs) get_openstreetmap_checkargs(args)
  format <- match.arg(format)
  if(format != 'png') stop('currently only the png format is supported.', call. = F)
  color <- match.arg(color)



  # url segments
  base_url <- 'http://tile.openstreetmap.org/cgi-bin/export?'
  bbox_url <- paste('bbox=', paste(bbox, collapse = ','), sep = '')
  scale_url <- paste('scale=', as.integer(scale), sep = '')
  format_url <- paste('format=', format, sep = '')


  # format url proper
  post_url <- paste(bbox_url, scale_url, format_url, sep = '&')
  url <- paste(base_url, post_url, sep = '')
  url <- URLencode(url)
  if(urlonly) return(url)

  # read in file
  destfile <- paste(filename, format, sep = '.')
  m <- try(download.file(url, destfile = destfile, quiet = !messaging, mode = 'wb'), silent = T)
  if(class(m) == 'try-error'){
    stop('map grabbing failed - see details in ?get_openstreetmap.',
      call. = FALSE)
  }
  map <- try(readPNG(destfile), silent = T)
  if(class(map) == 'try-error'){
    stop('map grabbing failed - see details in ?get_openstreetmap.',
      call. = FALSE)
  }

  # format file
  if(color == 'color'){
    map <- as.raster(apply(map, 2, rgb))
  } else if(color == 'bw'){
  	mapd <- dim(map)
  	map <- gray(.30 * map[,,1] + .59 * map[,,2] + .11 * map[,,3])
  	dim(map) <- mapd[1:2]
  	map <- as.raster(map)
  }
  class(map) <- c('ggmap','raster')

  # map spatial info
  attr(map, 'bb') <- data.frame(
    ll.lat = bbox[2], ll.lon = bbox[1],
    ur.lat = bbox[4], ur.lon = bbox[3]
  )

  # additional map meta-data
  attr(map, "source")  <- "osm"
  attr(map, "maptype") <- "openstreetmap"
  attr(map, "scale")    <- scale

  # return
  map
}


















get_openstreetmap_checkargs <- function(args){
  eargs <- lapply(args, eval)
  argsgiven <- names(args)

  with(eargs,{

    # bbox arg
    if('bbox' %in% argsgiven){
      if(!(is.numeric(bbox) && length(bbox) == 4)){
        stop('bounding box improperly specified.  see ?get_openstreetmap', call. = F)
      }
    }

    # scale arg
    if('scale' %in% argsgiven){
      if(!(is.numeric(scale) && length(scale) == 1 &&
      scale == round(scale) && scale > 0)){
        stop('scale must be a postive integer.', call. = F)
      }
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
