#' Single line segments with rounded ends
#'
#' This is ggplot2's segment with rounded ends.  It's mainly
#' included in ggmap for historical reasons.
#'
#' @inheritParams ggplot2::geom_segment
#' @seealso geom_segment in ggplot2, inspired by
#'   \url{http://spatialanalysis.co.uk/2012/02/great-maps-ggplot2/},
#'   \code{\link{route}}
#' @details only intended for use in ggmaps package.  only designed
#'   for mercator projection.
#' @export
#' @examples
#'
#' \dontrun{ # removed for R CMD check speed
#'
#' map <- get_map(
#'   location = c(-77.0425, 38.8925), # painfully picked by hand
#'   source = "google", zoom = 14, maptype = "satellite"
#' )
#' ggmap(map)
#'
#'
#' (legs_df <- route(
#'   "the white house, dc",
#'   "lincoln memorial washington dc",
#'   alternatives = TRUE
#' ))
#'
#' ggplot(data = legs_df) +
#'   geom_leg(aes(
#'     x = startLon, xend = endLon,
#'     y = startLat, yend = endLat
#'   )) +
#'   coord_map()
#'
#' ggplot(data = legs_df) +
#'   geom_leg(aes(
#'     x = startLon, xend = endLon,
#'     y = startLat, yend = endLat,
#'     color = route
#'   )) +
#'   coord_map()
#'
#'
#' ggmap(map) +
#'   geom_leg(
#'     aes(
#'       x = startLon, xend = endLon,
#'       y = startLat, yend = endLat
#'     ),
#'     data = legs_df, color = "red"
#'   )
#'
#' # adding a color aesthetic errors because of a base-layer problem
#' # ggmap(map) +
#' #   geom_leg(
#' #     aes(
#' #       x = startLon, xend = endLon,
#' #       y = startLat, yend = endLat,
#' #       color = route
#' #   )
#' # )
#'
#'
#' # this is probably the easiest hack to fix it
#' ggplot(data = legs_df) +
#'   inset_ggmap(map) +
#'   geom_leg(
#'     aes(
#'       x = startLon, xend = endLon,
#'       y = startLat, yend = endLat,
#'       color = route
#'     ),
#'     data = legs_df
#'   ) +
#'   coord_map()
#'
#' }
#'
geom_leg <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", arrow = NULL, lineend = "round",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomSegment,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      lineend = lineend,
      na.rm = na.rm,
      ...
    )
  )
}
