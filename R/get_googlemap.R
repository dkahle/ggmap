#' Get a Google Map.
#'
#' \code{get_googlemap} queries the Google Maps Static API version 2 to download
#' a static map.  Note that in most cases by using this function you are
#' agreeing to the Google Maps API Terms of Service at
#' \url{https://cloud.google.com/maps-platform/terms/}. Note that as of
#' mid-2018, registering with Google Cloud to obtain an API key is required to
#' use any of Google's services, including \code{get_googlemap}. Usage and
#' billing may apply, see the links under See Also further down in this
#' documentation for more details.
#'
#' @param center the center of the map; either a longitude/latitude numeric
#'   vector or a string containing a location, in which case
#'   \code{\link{geocode}} is called with \code{source = "google"}. (default:
#'   c(lon = -95.3632715, lat = 29.7632836), houston, texas)
#' @param zoom map zoom; an integer from 3 (continent) to 21 (building), default
#'   value 10 (city)
#' @param size rectangular dimensions of map in pixels - horizontal x vertical -
#'   with a max of c(640, 640). this parameter is affected in a multiplicative
#'   way by scale.
#' @param scale multiplicative factor for the number of pixels returned possible
#'   values are 1, 2, or 4 (e.g. size = c(640,640) and scale = 2 returns an
#'   image with 1280x1280 pixels).  4 is reserved for google business users
#'   only.  scale also affects the size of labels as well.
#' @param format character string providing image format - png, jpeg, and gif
#'   formats available in various flavors
#' @param maptype character string providing google map theme. options available
#'   are "terrain", "satellite", "roadmap", and "hybrid"
#' @param language character string providing language of map labels (for themes
#'   with them) in the format "en-EN".  not all languages are supported; for
#'   those which aren't the default language is used
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param filename destination file for download (file extension added according
#'   to format). Default \code{NULL} means a random \code{\link{tempfile}}.
#' @param color color or black-and-white
#' @param force if the map is on file, should a new map be looked up?
#' @param where where should the file drawer be located (without terminating
#'   "/")
#' @param archiving use archived maps.  note: by changing to TRUE you agree to
#'   the one of the approved uses listed in the Google Maps API Terms of Service
#'   : http://developers.google.com/maps/terms.
#' @param ext domain extension (e.g. "com", "co.nz")
#' @param inject character string to add to the url
#' @param region borders to display as a region code specified as a
#'   two-character ccTLD ("top-level domain") value, see
#'   \url{http://en.wikipedia.org/wiki/List_of_Internet_top-level_domains#Country_code_top-level_domains}
#' @param markers data.frame with first column longitude, second column
#'   latitude, for which google markers should be embedded in the map image, or
#'   character string to be passed directly to api
#' @param path data.frame (or list of data.frames) with first column longitude,
#'   second column latitude, for which a single path should be embedded in the
#'   map image, or character string to be passed directly to api
#' @param visible a location as a longitude/latitude numeric vector (or data
#'   frame with first column longitude, second latitude) or vector of character
#'   string addresses which should be visible in map extent
#' @param style character string to be supplied directly to the api for the
#'   style argument or a named vector (see examples). this is a powerful complex
#'   specification, see
#'   \url{https://developers.google.com/maps/documentation/staticmaps/}
#' @param ... ...
#' @return a ggmap object (a classed raster object with a bounding box
#'   attribute)
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso
#' \url{https://developers.google.com/maps/documentation/maps-static/intro},
#' \url{https://developers.google.com/maps/documentation/maps-static/dev-guide},
#' \url{https://developers.google.com/maps/documentation/maps-static/get-api-key},
#' \url{https://developers.google.com/maps/documentation/maps-static/usage-and-billing}
#' \code{\link{ggmap}}, \code{\link{register_google}}
#' @export
#' @examples
#'
#' \dontrun{ requires Google API key, see ?register_google
#'
#' ## basic usage
#' ########################################
#'
#' (map <- get_googlemap(c(-97.14667, 31.5493)))
#' ggmap(map)
#'
#' # plotting based on a colloquial name
#' # this requires a geocode() call, and needs that API
#' get_googlemap("waco, texas") %>% ggmap()
#'
#' # different maptypes are available
#' get_googlemap("waco, texas", maptype = "satellite") %>% ggmap()
#' get_googlemap("waco, texas", maptype = "hybrid") %>% ggmap()
#'
#' # you can get the url as follows
#' # see ?register_google if you want the key printed
#' get_googlemap(urlonly = TRUE)
#'
#'
#' ## other usage
#' ########################################
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
  center = c(lon = -95.3632715, lat = 29.7632836),
  zoom = 10,
  size = c(640,640),
  scale = 2,
  format = c("png8", "gif", "jpg", "jpg-baseline","png32"),
  maptype = c("terrain", "satellite", "roadmap", "hybrid"),
  language = "en-EN",
  messaging = FALSE,
  urlonly = FALSE,
  filename = NULL,
  color = c("color","bw"),
  force = FALSE,
  where = tempdir(),
  archiving = FALSE,
  ext = "com",
  inject = "",
  region,
  markers,
  path,
  visible,
  style,
  ...
){

  ## do argument checking
  ############################################################

  args <- as.list(match.call(expand.dots = TRUE)[-1])
  argsgiven <- names(args)

  if ("center" %in% argsgiven) {
    if (!( (is.numeric(center) && length(center) == 2) || (is.character(center) && length(center) == 1) )) {
      stop("center of map misspecified, see ?get_googlemap.", call. = FALSE)
    }
    if (all(is.numeric(center))) {
      lon <- center[1]; lat <- center[2]
      if (lon < -180 || lon > 180) stop("longitude of center must be between -180 and 180 degrees. note ggmap uses lon/lat, not lat/lon.", call. = FALSE)
      if (lat < -90 || lat > 90) stop("latitude of center must be between -90 and 90 degrees. note ggmap uses lon/lat, not lat/lon.", call. = FALSE)
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
  format <- match.arg(format)
  if(format != "png8") stop("currently only the png format is supported.", call. = FALSE)
  format0 <- sub("[[:digit:]]+|-.*", "", format)

  # maptype arg checked by match.arg
  maptype <- match.arg(maptype)

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
  if (!has_google_key() && !urlonly) stop("Google now requires an API key.", "\n       See ?register_google for details.", call. = FALSE)

  if(is.null(filename)){
    destfile <- tempfile(fileext = paste(".", format0, sep = ""))
  } else{
    filename_stop <- TRUE
    if(is.character(filename) && length(filename) == 1) filename_stop <- FALSE
    if(filename_stop) stop("improper filename specification, see ?get_googlemap.", call. = FALSE)
    destfile <- paste(filename, format0, sep = '.')
  }

  color <- match.arg(color)
  if(!missing(markers) && class(markers) == "list") markers <- list_to_dataframe(markers)
  if(!missing(path) && is.data.frame(path)) path <- list(path)




  ## construct url
  ############################################################

  # base_url <- sprintf("https://maps.googleapis.%s/maps/api/staticmap?", ext)
  url_base <- glue("https://maps.googleapis.{ext}/maps/api/staticmap?")

  center_url <- if (all(is.numeric(center))) { # lon/lat specification
    center <- round(center, digits = 6)
    lon <- center[1]; lat <- center[2]
    str_c("center=", str_c(lat, lon, sep = ","))
  } else { # address specification
    str_c("center=", URLencode( enc2utf8(center) ))
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
  if (has_google_client() && has_google_signature()) {
    client <- google_client()
    signature <- google_signature()
    post_url <- paste(post_url, fmteq(client), fmteq(signature), sep = "&")
  } else if (has_google_key()) {
    key <- google_key()
    post_url <- paste(post_url, fmteq(key), sep = "&")
  }

  # merge url and clean
  url <- str_c(url_base, post_url)
  url <- str_replace_all(url, "[&]+", "&") # removes missing arguments
  if (str_sub(url, -1L) == "&") url <- str_sub(url, end = -2L)

  # inject any remaining stuff
  if (inject != "") url <- str_c(url, inject, sep = "&")

  url <- URLencode( enc2utf8(url) )
  if(urlonly) if(showing_key()) return(url) else return(scrub_key(url))
  if(nchar(url) > 8192) stop("max url length is 8192 characters.", call. = FALSE)


  ## get map
  ############################################################

  # check to see if url is on file
  map <- file_drawer_get(url)
  if (!is.null(map) && !force) return(map)

  # message url
  if (showing_key()) message("Source : ", url) else message("Source : ", scrub_key(url))

  # query server
  response <- httr::GET(url)

  # deal with bad responses
  if (response$status_code != 200L) {
    warning(
      tryCatch(stop_for_status(response),
        "http_400" = function(c) "HTTP 400 Bad Request",
        "http_402" = function(c) "HTTP 402 Payment Required - May indicate over Google query limit",
        "http_403" = function(c) "HTTP 403 Forbidden - Server refuses, is the API enabled?",
        "http_404" = function(c) "HTTP 404 Not Found - Server reports page not found",
        "http_414" = function(c) "HTTP 414 URI Too Long - URL query too long",
        "http_500" = function(c) "HTTP 500 Internal Server Error",
        "http_503" = function(c) "HTTP 503 Service Unavailable - Server bogged down, try later"
      )
    )
  }


  ## read in map and format, add meta data
  ############################################################

  map <- httr::content(response)
  map <- aperm(map, c(2, 1, 3))

  # format file
  if(color == "color"){
    map <- apply(map, 2, rgb)
  } else if(color == "bw"){
  	mapd <- dim(map)
  	map <- gray(.30 * map[,,1] + .59 * map[,,2] + .11 * map[,,3])
  	dim(map) <- mapd[1:2]
  }

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

  # additional map meta-data
  map <- structure(
    map,
    "class" = c("ggmap","raster"),
    "source" = "google",
    "maptype" = maptype,
    "zoom" = zoom,
    "bb" = tibble(
      "ll.lat" = ll[1], "ll.lon" = ll[2],
      "ur.lat" = ur[1], "ur.lon" = ur[2]
    )
  )

  # archive map for future use
  if (archiving) file_drawer_set(url, map)

  # kick out
  map
}




















