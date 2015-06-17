#' Convert a bb specification to a bbox specification
#'
#' In ggmap, all maps (class ggmap) have the bb attribute,
#' a data frame bounding box specification in terms of the
#' bottom left and top right points of the spatial extent.
#' This function converts this specification to a named
#' double vector (with names left, bottom, right, top)
#' specification that is used in some querying functions
#' (e.g. get_stamenmap).
#'
#' @param bb a bounding box in bb format (see examples)
#' @return a bounding box in bbox format (see examples)
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @export
#' @examples
#'
#' gc <- geocode("statue of liberty", source = "google")
#'
#' googMap <- get_googlemap(center = as.numeric(gc))
#' (bb <- attr(googMap, "bb"))
#' bb2bbox(bb)
#'
#' stamMap <- get_stamenmap(bb2bbox(bb))
#'
#' ggmap(googMap) +
#'   geom_point(
#'     aes(x = lon, y = lat),
#'     data = gc, colour = "red", size = 3
#'   )
#'
#' ggmap(stamMap) +
#'   geom_point(
#'     aes(x = lon, y = lat),
#'     data = gc, colour = "red", size = 3
#'   )
#'
bb2bbox <- function(bb){
  with(bb,
    c(
      left = ll.lon, bottom = ll.lat,
      right = ur.lon, top = ur.lat
    )
  )
}
