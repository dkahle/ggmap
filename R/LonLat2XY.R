#' Convert a lon/lat coordinate to a tile coordinate
#'
#' Convert a lon/lat coordinate to a tile coordinate for a given
#' zoom.  Decimal tile coordinates (x, y) are reported.
#'
#' @param lon_deg longitude in degrees
#' @param lat_deg latitude in degrees
#' @param zoom zoom
#' @param xpix width of tile in pixels
#' @param ypix length of tile in pixels
#' @return a data frame with columns X, Y, x, y
#' @author David Kahle \email{david.kahle@@gmail.com}, based on
#'   function LatLon2XY by Markus Loecher, Sense Networks
#'   \email{markus@@sensenetworks.com} in package RgoogleMaps
#' @seealso
#'   \url{http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames}
#' @export
#' @examples
#'
#'
#' \dontrun{
#' gc <- geocode('baylor university')
#' LonLat2XY(gc$lon, gc$lat, 10)
#'
#' }
#'
LonLat2XY <- function(lon_deg, lat_deg, zoom, xpix=256, ypix=256){
  n <- 2^zoom
  X <- ((lon_deg + 180) / 360) * n
  sec <- function(x) 1/cos(x)
  lat_rad <- lat_deg * pi/180
  Y <- (1 - (log(tan(lat_rad) + sec(lat_rad)) / pi)) / 2 * n
  df <- data.frame(
    X = floor(X),
    Y = floor(Y),
    x = xpix*(X - floor(X)),
    y = ypix*(Y - floor(Y))
  )
  row.names(df) <- NULL
  df
}
