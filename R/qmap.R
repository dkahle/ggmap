#' Quick map plot
#'
#' qmap is a wrapper for [ggmap()] and [get_map()].
#'
#' @param location character; location of interest
#' @param ... stuff to pass to [ggmap()] and [get_map()]
#' @return a ggplot object
#' @author David Kahle \email{david@@kahle.io}
#' @seealso [ggmap()] and [get_map()]
#' @export
#' @examples
#'
#' \dontrun{ some requires Google API key; heavy network/time load
#'
#' location <- "marrs mclean science, waco, texas"
#' qmap(location)
#' qmap(location, zoom = 14)
#' qmap(location, zoom = 14, source = "osm")
#' qmap(location, zoom = 14, source = "osm", scale = 20000)
#' qmap(location, zoom = 14, maptype = "satellite")
#' qmap(location, zoom = 14, maptype = "hybrid")
#' qmap(location, zoom = 14, maptype = "toner", source = "stamen")
#' qmap(location, zoom = 14, maptype = "watercolor", source = "stamen")
#' qmap(location, zoom = 14, maptype = "terrain-background", source = "stamen")
#' qmap(location, zoom = 14, maptype = "toner-lite", source = "stamen")
#'
#' where <- "the white house, washington dc"
#' wh <- geocode(where)
#' qmap(where, maprange = TRUE, zoom = 15,
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
    # address
    location_stop <- FALSE
  }
  if(is.numeric(location) && length(location) == 2){
    # lonlat
    location_stop <- FALSE
  }
  if(is.numeric(location) && length(location) == 4){
    # bbox
    location_stop <- FALSE
  }
  if(location_stop){
    cli::cli_abort("{.arg location} improperly specified, see {.fn ggmap::get_map}")
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
      color = color, maptype = maptype, language = language, force = force),
    maprange = maprange, extent = extent, base_layer = base_layer, legend = legend,
      padding = padding, darken = darken
  )
}
