#' Quick map plot
#'
#' qmap is a wrapper for \code{\link{ggmap}} and \code{\link{get_map}}.
#'
#' @param location character; location of interest
#' @param ... stuff to pass to \code{\link{ggmap}} and \code{\link{get_map}}.
#' @return a ggplot object
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{ggmap}} and \code{\link{get_map}}.
#' @export
#' @examples
#'
#' \dontrun{
#' # these examples have been excluded for checking efficiency
#'
#' qmap(location = "baylor university")
#' qmap(location = "baylor university", zoom = 14)
#' qmap(location = "baylor university", zoom = 14, source = "osm")
#' qmap(location = "baylor university", zoom = 14, source = "osm", scale = 20000)
#' qmap(location = "baylor university", zoom = 14, maptype = "satellite")
#' qmap(location = "baylor university", zoom = 14, maptype = "hybrid")
#' qmap(location = "baylor university", zoom = 14, maptype = "toner", source = "stamen")
#' qmap(location = "baylor university", zoom = 14, maptype = "watercolor", source = "stamen")
#' qmap(location = "baylor university", zoom = 14, maptype = "terrain-background", source = "stamen")
#' qmap(location = "baylor university", zoom = 14, maptype = "toner-lite", source = "stamen")
#'
#' api_key <- "<your api key here>"
#' qmap(location = "baylor university", zoom = 14, maptype = 15434,
#'   source = "cloudmade", api_key = api_key)
#'
#' wh <- geocode("the white house")
#' qmap("the white house", maprange = TRUE,
#'   base_layer = ggplot(aes(x=lon, y=lat), data = wh)) +
#'   geom_point()
#'
#'
#'
#' }
#'
qmap <- function(location = "houston", ...){

  # location formatting
  location_stop <- TRUE
  if(is.character(location) && length(location) == 1){
    location_type <- "address"
    location_stop <- FALSE
  }
  if(is.numeric(location) && length(location) == 2){
    location_type <- "lonlat"
    location_stop <- FALSE
  }
  if(is.numeric(location) && length(location) == 4){
    location_type <- "bbox"
    location_stop <- FALSE
  }
  if(location_stop){
    stop("improper location specification, see ?get_map.", call. = F)
  }

  args <- as.list(match.call(expand.dots = TRUE)[-1])

  # get_map args
  ##################

  if("zoom" %in% names(args)){
    zoom <- eval(args$zoom)
  } else {
  	zoom <- 10
  }

  if("scale" %in% names(args)){
    scale <- eval(args$scale)
  } else {
  	scale <- "auto"
  }

  if("messaging" %in% names(args)){
    messaging <- eval(args$messaging)
  } else {
    messaging <- FALSE
  }

  if("urlonly" %in% names(args)){
    urlonly <- eval(args$urlonly)
  } else {
    urlonly <- FALSE
  }

  if("filename" %in% names(args)){
    filename <- eval(args$filename)
  } else {
    filename <- "ggmapTemp"
  }

  if("color" %in% names(args)){
    color <- eval(args$color)
  } else {
    color <- "color"
  }

  if("source" %in% names(args)){
    source <- eval(args$source)
  } else {
    source <- "google"
  }

  if("force" %in% names(args)){
    force <- eval(args$force)
  } else {
    if(source == "google"){
      force <- FALSE #TRUE
    } else {
      force <- FALSE
    }
  }

  if("maptype" %in% names(args)){
    maptype <- eval(args$maptype)
  } else {
  	if(source != "cloudmade"){
      maptype <- "terrain"
    } else {
      maptype <- 1
    }
  }

  if("crop" %in% names(args)){
    crop <- eval(args$crop)
  } else {
    crop <- TRUE
  }

  if("api_key" %in% names(args)){
    api_key <- eval(args$api_key)
  } else {
    if(source == "cloudmade"){
      stop("an api key must be specified for cloudmade maps, see ?get_cloudmademap.",
      call. = F)
    }
    api_key <- NULL
  }




  # ggmap args
  ##################

  if("extent" %in% names(args)){
    extent <- eval(args$extent)
  } else {
  	extent <- "device"
  }

  if("maprange" %in% names(args)){
    maprange <- eval(args$maprange)
  } else {
    maprange <- FALSE
  }

  if("base_layer" %in% names(args)){
    base_layer <- args$base_layer
  } else {
    base_layer <- "auto"
  }

  if("legend" %in% names(args)){
    legend <- args$legend
  } else {
    legend <- "right"
  }

  if("padding" %in% names(args)){
    padding <- args$padding
  } else {
    padding <- .02
  }

  if("darken" %in% names(args)){
    darken <- eval(args$darken)
  } else {
    darken <- c(0, "black")
  }

  if("language" %in% names(args)){
    language <- eval(args$language)
  } else {
  	language <- "en-EN"
  }



  #ggmap(get_map(location = location, ...), ...)
  # return
  ggmap(
    get_map(location = location, zoom = zoom, scale = scale, source = source,
      color = color, maptype = maptype, language = language, api_key = api_key, force = force),
    maprange = maprange, extent = extent, base_layer = base_layer, legend = legend,
      padding = padding, darken = darken
  )
}
