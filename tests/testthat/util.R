getFakeMap <- function() {
  map <- rep("#FFFFFF", 4)
  dim(map) <- c(2, 2)

  class(map) <- c('ggmap','raster')
  attr(map, "source")  <- "osm"
  attr(map, "maptype") <- "openstreetmap"
  attr(map, "scale") <- 606250
  attr(map, 'bb') <- data.frame(
    ll.lat = 1, ll.lon = 2,
    ur.lat = 3, ur.lon = 4
  )
  map
}
