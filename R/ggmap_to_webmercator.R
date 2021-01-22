#' \code{ggmap_to_webmercator} change bounding box to match with sf objects (CRS needs to be 3857)
#'
#' @param map The map to update

#' @export
#' @rdname ggmap_to_webmercator
#' @import sf
#' @importFrom stats setNames
#'
ggmap_to_webmercator <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")),
                       c("ymin", "xmin", "ymax", "xmax"))

  # Coonvert the bbox to an sf polygon, transform it to 3857,
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(map_bbox, crs = 4326)), 3857))

  # Overwrite the bbox of the ggmap object with the transformed coordinates
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]

  map
}
