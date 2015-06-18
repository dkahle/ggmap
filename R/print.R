#' Print a map
#'
#' Print a console description of a map
#'
#' @param x an object of class elicit
#' @param ... additional parameters
#' @usage \method{print}{ggmap}(x, ...)
#' @return Invisible string of the printed object.
#' @export
#' @examples
#'
#' get_map()
#' ggmap(get_map())
#'
#'
print.ggmap <- function(x, ...){

  r <- nrow(x)
  c <- ncol(x)

  source <- attr(x, "source")
  if(source == "google"){
    source <- "Google Maps"
  } else if(source == "stamen"){
    source <- "Stamen Maps"
  } else if(source == "osm"){
    source <- "OpenStreetMap"
  } else if(source == "cloudmade"){
    source <- "Cloudmade"
  } else if(source == "naver"){
    source <- "Naver Map"
  }

  cat(paste0(r, "x", c, " ", attr(x, "maptype"), " map image from ", source, ".",
             "  see ?ggmap to plot it."), ...)

  invisible()
}

