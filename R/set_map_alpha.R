#' Add transparency to a ggmap object
#'
#' Add an alpha (transparency) layer to a \code{ggmap} object. This makes it
#' possible to superimpose multiple map layers and have the lower layers shine
#' through.
#'
#' @param map A \code{ggmap} object.
#' @param alpha The required transparency (a number between 0 and 1).
#'
#' @return A \code{ggmap} object.
#' @export
#'
#' @examples
#' zoom <- 12
#' bbox <- c(-0.3275, 51.407222, 0.0725, 51.607222)
#'
#' base <- get_map(bbox, zoom, maptype = "terrain-lines")
#' overlay <- get_map(bbox, zoom, maptype = "watercolor")
#'
#' # Plot the base map.
#' ggmap(base)
#' # Plot the overlay map (100% opacity).
#' ggmap(overlay)
#'
#' # Plot the base map with the overlay superimposed at 25% opacity.
#' ggmap(base) +
#'   inset_ggmap(
#'     set_map_alpha(overlay, 0.25)
#'   )
set_map_alpha <- function(map, alpha) {
  if(class(map)[1] != "ggmap") {
    stop("map must be a ggmap object", call. = FALSE)
  }
  if(class(alpha) != "numeric" || alpha < 0 || alpha > 1) {
    stop("alpha must be a number between 0 and 1", call. = FALSE)
  }

  # Record the attributes & dimensions of the map object.
  map_attributes <- attributes(map)
  map_dimensions <- dim(map)

  # Add an alpha channel.
  map <- grDevices::adjustcolor(map, alpha)

  # Add back the dimensions (convert vector to matrix).
  dim(map) <- map_dimensions
  # Add back the attributes.
  attributes(map) <- map_attributes

  # Convert from matrix to map.
  class(map) <- c("ggmap", "raster")

  map
}
