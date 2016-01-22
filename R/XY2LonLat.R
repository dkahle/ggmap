#' Convert a tile coordinate to a lon/lat coordinate
#'
#' Convert a tile coordinate to a lon/lat coordinate for a given
#' zoom.  Decimal tile coordinates are accepted.
#'
#' @param X horizontal map-tile coordinate (0 is map-left)
#' @param Y vertical map-tile coordinate (0 is map-top)
#' @param zoom zoom
#' @param x within tile x (0 is tile-left)
#' @param y within tile y (0 it tile-top)
#' @param xpix width of tile in pixels
#' @param ypix length of tile in pixels
#' @return a data frame with columns lon and lat (in degrees)
#' @author David Kahle \email{david.kahle@@gmail.com}, based on
#'   function XY2LatLon by Markus Loecher, Sense Networks
#'   \email{markus@@sensenetworks.com} in package RgoogleMaps
#' @seealso
#' \url{http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames}
#' @export
#' @examples
#'
#'
#' \dontrun{
#' XY2LonLat(480, 845, zoom = 11)
#' XY2LonLat(0, 0, zoom = 1)
#' XY2LonLat(0, 0, 255, 255, zoom = 1)
#' XY2LonLat(0, 0, 255, 255, zoom = 1)
#'
#' }
#'
XY2LonLat <- function(X, Y, zoom, x = 0, y = 0, xpix=255, ypix=255){
  n <- 2^zoom
  lon_deg <- (X+x/xpix) / n * 360.0 - 180.0
  tmp <- tanh( pi * (1 - 2 * (Y+y/ypix) / n))
  ShiftLat <- function(tmp) {
    lat <- 2 * pi * (-1:1) + asin(tmp)
    lat[which(-pi/2 < lat & lat <= pi/2)] * 180/pi
  }
  lat_deg <- ShiftLat(tmp)
  data.frame(lon = lon_deg, lat = lat_deg)
}
