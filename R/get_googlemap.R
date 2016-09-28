#' Get a Google Map.
#'
#' \code{get_googlemap} accesses the Google Static Maps API version
#' 2 to download a static map.  Note that in most cases by using
#' this function you are agreeing to the Google Maps API Terms of
#' Service at \url{https://developers.google.com/maps/terms}.
#'
#' @param center the center of the map.  Either a longitude/latitude
#'   numeric vector, a string address (note that the latter uses
#'   \code{geocode} with \code{source = "google"}).
#' @param zoom map zoom, an integer from 3 (continent) to 21
#'   (building), default value 10 (city)
#' @param size rectangular dimensions of map in pixels - horizontal
#'   x vertical - with a max of c(640, 640). this parameter is
#'   affected in a multiplicative way by scale.
#' @param scale multiplicative factor for the number of pixels
#'   returned possible values are 1, 2, or 4 (e.g. size = c(640,640)
#'   and scale = 2 returns an image with 1280x1280 pixels).  4 is
#'   reserved for google business users only.  scale also affects
#'   the size of labels as well.
#' @param format character string providing image format - png,
#'   jpeg, and gif formats available in various flavors
#' @param maptype character string providing google map theme.
#'   options available are "terrain", "satellite", "roadmap", and
#'   "hybrid"
#' @param language character string providing language of map labels
#'   (for themes with them) in the format "en-EN".  not all
#'   languages are supported; for those which aren't the default
#'   language is used
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param filename destination file for download (file extension
#'   added according to format)
#' @param color color or black-and-white
#' @param force if the map is on file, should a new map be looked
#'   up?
#' @param where where should the file drawer be located (without
#'   terminating "/")
#' @param archiving use archived maps.  note: by changing to TRUE
#'   you agree to the one of the approved uses listed in the Google
#'   Maps API Terms of Service :
#'   http://developers.google.com/maps/terms.
#' @param ext domain extension (e.g. "com", "co.nz")
#' @param inject character string to add to the url
#' @param region borders to display as a region code specified as a
#'   two-character ccTLD ("top-level domain") value, see
#'   \url{http://en.wikipedia.org/wiki/List_of_Internet_top-level_domains#Country_code_top-level_domains}
#' @param markers data.frame with first column longitude, second
#'   column latitude, for which google markers should be embedded in
#'   the map image, or character string to be passed directly to api
#' @param path data.frame (or list of data.frames) with first column
#'   longitude, second column latitude, for which a single path
#'   should be embedded in the map image, or character string to be
#'   passed directly to api
#' @param visible a location as a longitude/latitude numeric vector
#'   (or data frame with first column longitude, second latitude) or
#'   vector of character string addresses which should be visible in
#'   map extent
#' @param style character string to be supplied directly to the api
#'   for the style argument or a named vector (see examples). this
#'   is a powerful complex specification, see
#'   \url{https://developers.google.com/maps/documentation/staticmaps/}
#' @param ... ...
#' @return a ggmap object (a classed raster object with a bounding
#'   box attribute)
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso
#' \url{https://developers.google.com/maps/documentation/staticmaps/},
#' \code{\link{ggmap}}, \code{\link{register_google}}
#' @export
#' @examples
#'
#' \dontrun{ # to diminish run check time
#'
#'
#' get_googlemap(urlonly = TRUE)
#' ggmap(get_googlemap())
#'
#'
#' # markers and paths are easy to access
#' d <- function(x=-95.36, y=29.76, n,r,a){
#'   round(data.frame(
#'     lon = jitter(rep(x,n), amount = a),
#'     lat = jitter(rep(y,n), amount = a)
#'   ), digits = r)
#' }
#' df <- d(n=50,r=3,a=.3)
#' map <- get_googlemap(markers = df, path = df, scale = 2)
#' ggmap(map)
#' ggmap(map, extent = "device") +
#'   geom_point(aes(x = lon, y = lat), data = df, size = 3, colour = "black") +
#'   geom_path(aes(x = lon, y = lat), data = df)
#'
#' gc <- geocode("waco, texas", source = "google")
#' center <- as.numeric(gc)
#' ggmap(get_googlemap(center = center, color = "bw", scale = 2), extent = "device")
#'
#' # the scale argument can be seen in the following
#' # (make your graphics device as large as possible)
#' ggmap(get_googlemap(center, scale = 1), extent = "panel") # pixelated
#' ggmap(get_googlemap(center, scale = 2), extent = "panel") # fine
#'
#' # archiving; note that you must meet google's terms for this condition
#' map <- get_googlemap(archiving = TRUE)
#' map <- get_googlemap()
#' ggmap(map)
#'
#'
#' # style
#' map <- get_googlemap(
#'   maptype = "roadmap",
#'   style = c(feature = "all", element = "labels", visibility = "off"),
#'   color = "bw"
#' )
#' ggmap(map)
#'
#'
#'
#'
#' }
#'
get_googlemap <- function(
  center = c(lon = -95.3632715, lat = 29.7632836), zoom = 10, size = c(640,640),
  scale = 2, format = c("png8", "gif", "jpg", "jpg-baseline","png32"),
  maptype = c("terrain", "satellite", "roadmap", "hybrid"),
  language = "en-EN",
  messaging = FALSE, urlonly = FALSE, filename = "ggmapTemp", color = c("color","bw"),
  force = FALSE, where = tempdir(), archiving = FALSE,
  ext = "com", inject = "",
  region, markers, path, visible, style, ...
){

  ##### do argument checking
  ############################################################

  args <- as.list(match.call(expand.dots = TRUE)[-1])
  argsgiven <- names(args)

  if("center" %in% argsgiven){
    if(!(
      (is.numeric(center) && length(center) == 2) ||
      (is.character(center) && length(center) == 1)
    )){
      stop("center of map misspecified, see ?get_googlemap.", call. = FALSE)
    }
    if(all(is.numeric(center))){
      lon <- center[1]; lat <- center[2]
      if(lon < -180 || lon > 180){
        stop("longitude of center must be between -180 and 180 degrees.",
          " note ggmap uses lon/lat, not lat/lon.", call. = FALSE)
      }
      if(lat < -90 || lat > 90){
        stop("latitude of center must be between -90 and 90 degrees.",
          " note ggmap uses lon/lat, not lat/lon.", call. = FALSE)
      }
    }
  }

  if("zoom" %in% argsgiven){
    if(!(is.numeric(zoom) && zoom == round(zoom) && zoom > 0)){
      stop("zoom must be a whole number between 1 and 21", call. = FALSE)
    }
  }

  if("size" %in% argsgiven){
    stopifnot(all(is.numeric(size)) && all(size == round(size)) && all(size > 0))
  }

  if("scale" %in% argsgiven) stopifnot(scale %in% c(1,2,4))

  # format arg checked by match.arg
  # maptype arg checked by match.arg

  if("markers" %in% argsgiven){
    markers_stop <- TRUE
    if(is.data.frame(markers) && all(apply(markers[,1:2],2,is.numeric))) markers_stop <- FALSE
    if(
      class(markers) == "list" &&
      all(sapply(markers, function(elem){
        is.data.frame(elem) && all(apply(elem[,1:2],2,is.numeric))
      }))
    ) markers_stop <- FALSE
    if(is.character(markers) && length(markers) == 1) markers_stop <- FALSE

    if(markers_stop) stop("improper marker specification, see ?get_googlemap.", call. = FALSE)
  }

  if("path" %in% argsgiven){
    path_stop <- TRUE
    if(is.data.frame(path) && all(apply(path[,1:2],2,is.numeric))) path_stop <- FALSE
    if(
      class(path) == "list" &&
      all(sapply(path, function(elem){
        is.data.frame(elem) && all(apply(elem[,1:2],2,is.numeric))
      }))
    ) path_stop <- FALSE
    if(is.character(path) && length(path) == 1) path_stop <- FALSE

    if(path_stop) stop("improper path specification, see ?get_googlemap.", call. = FALSE)
  }

  if("visible" %in% argsgiven){
    message("visible argument untested.")
    visible_stop <- TRUE
    if(is.data.frame(visible) && all(apply(visible[,1:2],2,is.numeric))) visible_stop <- FALSE
    if(is.character(visible)) visible_stop <- FALSE
    if(visible_stop) stop("improper visible specification, see ?get_googlemap.", call. = FALSE)
  }

  if("style" %in% argsgiven){
    style_stop <- TRUE
    if(is.list(style)) style <- unlist(style)
    if(is.character(style)){
      if(length(style) > 1){
        style <- paste(
          paste(names(style), style, sep = ":"),
          collapse = "|"
        )
      }
      style_stop <- FALSE
    }
    if(style_stop) stop("improper style specification, see ?get_googlemap.", call. = FALSE)
  }

  # if(   "sensor" %in% argsgiven) stopifnot(is.logical(   sensor))
  if("messaging" %in% argsgiven) stopifnot(is.logical(messaging))
  if(  "urlonly" %in% argsgiven) stopifnot(is.logical(  urlonly))

  if("filename" %in% argsgiven){
    filename_stop <- TRUE
    if(is.character(filename) && length(filename) == 1) filename_stop <- FALSE
    if(filename_stop) stop("improper filename specification, see ?get_googlemap.", call. = FALSE)

  }

  # argument checking (no checks for language, region, markers, path, visible, style)
  #args <- as.list(match.call(expand.dots = TRUE)[-1])
  #if(checkargs) get_googlemap_checkargs(args)
  format <- match.arg(format)
  if(format != "png8") stop("currently only the png format is supported.", call. = FALSE)
  maptype <- match.arg(maptype)
  color <- match.arg(color)
  if(!missing(markers) && class(markers) == "list") markers <- list_to_dataframe(markers)
  if(!missing(path) && is.data.frame(path)) path <- list(path)



  ##### construct url
  ############################################################

  base_url <- sprintf("https://maps.googleapis.%s/maps/api/staticmap?", ext)
  center_url <- if(all(is.numeric(center))){ # lon/lat specification
    center <- round(center, digits = 6)
    lon <- center[1]; lat <- center[2]
    paste0("center=", paste(lat,lon,sep = ","))
  } else { # address specification
    centerPlus <- gsub(" ", "+", center)
    paste0("center=", centerPlus)
  }

  size_url <- fmteq(size, paste, collapse = "x")
  format_url <- if(!missing(format) && format != "png8"){ fmteq(format) } else { "" }
  language_url <- if(!missing(language)){ fmteq(language) } else { "" }
  region_url <- if(!missing(region)){ fmteq(region) } else { "" }

  markers_url <- if(!missing(markers)){
    	if(is.data.frame(markers)){
    	  markers <- apply(markers, 1, function(v) paste(rev(round(v,6)), collapse = ","))
    	  fmteq(markers, paste, collapse = "|")
      } else {
        fmteq(markers)
      }
    } else { "" }

  path_url <- if(!missing(path)){
      if(is.list(path)){
      	ps <- vapply(path, function(one_path){
      	  path <- apply(one_path, 1, function(v) paste(rev(round(v,6)), collapse = ","))
      	  fmteq(path, paste, collapse = "|")
      	}, character(1))
      	paste(ps, collapse = "&")
      } else {
        fmteq(path)
      }
    } else { "" }

  visible_url <- if(!missing(visible)){
      if(is.data.frame(visible)){
        visible <- apply(visible, 1, function(v) paste(rev(round(v,6)), collapse = ","))
        fmteq(visible, paste0, collapse = "|")
      } else {
        fmteq(visible, paste0, collapse = "|")
      }
    } else { "" }

  style_url <- if(!missing(style)){ fmteq(style) } else { "" }
  # sensor_url <- fmteq(sensor, function(x) tolower(as.character(x)))


  # format url proper
  post_url <- paste(center_url, fmteq(zoom), size_url, fmteq(scale),
    format_url, fmteq(maptype), language_url, region_url, markers_url,
    path_url, visible_url, style_url, # sensor_url, # sensor no longer required
    sep = "&")

  # add google account stuff
  if (has_goog_client() && has_goog_signature()) {
    client <- goog_client()
    signature <- goog_signature()
    post_url <- paste(post_url, fmteq(client), fmteq(signature), sep = "&")
  } else if (has_goog_key()) {
    key <- goog_key()
    post_url <- paste(post_url, fmteq(key), sep = "&")
  }

  url <- paste0(base_url, post_url)
  url <- gsub("[&]+","&",url) # removes missing arguments
  if(substr(url, nchar(url), nchar(url)) == "&"){ # if ends with &
    url <- substr(url, 1, nchar(url)-1)
  }

  # inject any remaining stuff
  if(inject != "") url <- paste(url, inject, sep = "&")

  url <- URLencode( enc2utf8(url) )
  if(urlonly) return(url)
  if(nchar(url) > 2048) stop("max url length is 2048 characters.", call. = FALSE)


  ##### get map
  ############################################################

  # check to see if url is on file
  map <- file_drawer_get(url)
  if (!is.null(map) && !force) return(map)

  # finalize filename
  tmp <- tempfile()
  download.file(url, destfile = tmp, quiet = !messaging, mode = "wb")
  message(paste0("Source : ", url))


  ##### read in map and format, add meta data
  ############################################################

  map <- readPNG(tmp)
  map <- aperm(map, c(2, 1, 3))

  # format file
  if(color == "color"){
    map <- apply(map, 2, rgb)
  } else if(color == "bw"){
  	mapd <- dim(map)
  	map <- gray(.30 * map[,,1] + .59 * map[,,2] + .11 * map[,,3])
  	dim(map) <- mapd[1:2]
  }
  map <- matrix(map, nrow = scale*size[2], ncol = scale*size[1])
  class(map) <- c("ggmap","raster")
  # plot(map)

  # map spatial info
  if(is.character(center)) center <- as.numeric(geocode(center, source = "google"))
  ll <- XY2LatLon(
    list(lat = center[2], lon = center[1], zoom = zoom),
    -size[1]/2 + 0.5,
    -size[2]/2 - 0.5
  )
  ur <- XY2LatLon(
    list(lat = center[2], lon = center[1], zoom = zoom),
    size[1]/2 + 0.5,
    size[2]/2 - 0.5
  )
  attr(map, "bb") <- data.frame(
    ll.lat = ll[1], ll.lon = ll[2],
    ur.lat = ur[1], ur.lon = ur[2]
  )

  # additional map meta-data
  attr(map, "source")  <- "google"
  attr(map, "maptype") <- maptype
  attr(map, "zoom")    <- zoom

  # transpose
  out <- map # t(map)

  # archive map for future use
  fileNameCenter <- as.character(center)
  fileNameCenter <- paste0(gsub("\\.", "_", fileNameCenter),collapse = "-")
  if (archiving) file_drawer_set(url, out)

  # kick out
  out
}








































get_googlemap_checkargs <- function(args){
  eargs <- lapply(args, eval)
  argsgiven <- names(args)

  with(eargs,{

    # center arg
    if("center" %in% argsgiven){
      if(!(
        (is.numeric(center) && length(center) == 2) ||
        (is.character(center) && length(center) == 1)
      )){
        stop("center of map misspecified, see ?get_googlemap.", call. = FALSE)
      }
      if(all(is.numeric(center))){
        lon <- center[1]; lat <- center[2]
        if(lon < -180 || lon > 180){
          stop("longitude of center must be between -180 and 180 degrees.",
            " note ggmap uses lon/lat, not lat/lon.", call. = FALSE)
        }
        if(lat < -90 || lat > 90){
          stop("latitude of center must be between -90 and 90 degrees.",
            " note ggmap uses lon/lat, not lat/lon.", call. = FALSE)
        }
      }
    }

    # zoom arg
    if("zoom" %in% argsgiven){
      if(!(is.numeric(zoom) && zoom == round(zoom) && zoom > 0)){
        stop("zoom must be a whole number between 1 and 21", call. = FALSE)
      }
    }

    # size arg
    if("size" %in% argsgiven){
      stopifnot(all(is.numeric(size)) && all(size == round(size)) && all(size > 0))
    }

    # scale arg
    if("scale" %in% argsgiven) stopifnot(scale %in% c(1,2,4))

    # format arg checked by match.arg

    # maptype arg checked by match.arg

    # markers arg (optional)
    if("markers" %in% argsgiven){
      markers_stop <- TRUE
      if(is.data.frame(markers) && all(apply(markers[,1:2],2,is.numeric))) markers_stop <- FALSE
      if(
        class(markers) == "list" &&
        all(sapply(markers, function(elem){
          is.data.frame(elem) && all(apply(elem[,1:2],2,is.numeric))
        }))
      ) markers_stop <- FALSE
      if(is.character(markers) && length(markers) == 1) markers_stop <- FALSE

      if(markers_stop) stop("improper marker specification, see ?get_googlemap.", call. = FALSE)
    }

    # path arg (optional)
    if("path" %in% argsgiven){
      path_stop <- TRUE
      if(is.data.frame(path) && all(apply(path[,1:2],2,is.numeric))) path_stop <- FALSE
      if(
        class(path) == "list" &&
        all(sapply(path, function(elem){
          is.data.frame(elem) && all(apply(elem[,1:2],2,is.numeric))
        }))
      ) path_stop <- FALSE
      if(is.character(path) && length(path) == 1) path_stop <- FALSE

      if(path_stop) stop("improper path specification, see ?get_googlemap.", call. = FALSE)
    }

    # visible arg (optional)
    if("visible" %in% argsgiven){
      message("visible argument untested.")
      visible_stop <- TRUE
      if(is.data.frame(visible) && all(apply(visible[,1:2],2,is.numeric))) visible_stop <- FALSE
      if(is.character(visible)) visible_stop <- FALSE
      if(visible_stop) stop("improper visible specification, see ?get_googlemap.", call. = FALSE)
    }

    # style arg (optional)
    if("style" %in% argsgiven){
      message("style argument untested.")
      style_stop <- TRUE
      if(is.character(style) && length(style) == 1) style_stop <- FALSE
      if(style_stop) stop("improper style specification, see ?get_googlemap.", call. = FALSE)
    }

    # sensor, messaging, urlonly args
    # if("sensor" %in% argsgiven) stopifnot(is.logical(sensor))
    if("messaging" %in% argsgiven) stopifnot(is.logical(messaging))
    if("urlonly" %in% argsgiven) stopifnot(is.logical(urlonly))

    # filename arg
    if("filename" %in% argsgiven){
      filename_stop <- TRUE
      if(is.character(filename) && length(filename) == 1) filename_stop <- FALSE
      if(filename_stop) stop("improper filename specification, see ?get_googlemap.", call. = FALSE)
    }

    # color arg checked by match.arg

  }) # end with
}
