#' Convert a lon/lat coordinate to a tile coordinate
#'
#' @param lon_deg, lat_deg longitude and latitutde in degrees
#' @param zoom zoom
#' @param xpix,ypix height and width of tile in pixels
#' @return A data frame with columns X, Y (tile), x, y (position within tile)
#' @author David Kahle \email{david.kahle@@gmail.com}, based on function LatLon2XY by Markus Loecher, Sense Networks \email{markus@@sensenetworks.com} in package RgoogleMaps
#' @seealso \url{http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames}
#' @export
#' @examples
#' baylor <- list(lon = -97.11844, lat = 31.54822)
#' LonLat2XY(baylor$lon, baylor$lat, 10)
LonLat2XY <- function(lon_deg, lat_deg, zoom, xpix=256, ypix=256){
  n <- 2^zoom
  X <- ((lon_deg + 180) / 360) * n
  lat_rad <- lat_deg * pi/180
  Y <- (1 - (log(tan(lat_rad) + sec(lat_rad)) / pi)) / 2 * n
  df <- data.frame(
    X = floor(X),
    Y = floor(Y),
    x = xpix*(X - floor(X)),
    y = xpix*(Y - floor(Y))
  )
  row.names(df) <- NULL
  df
}

lon2x <- function(lon, zoom) {
  n <- 2 ^ zoom

  X <- ((lon + 180) / 360) * n
  data.frame(
    X = floor(X),
    x = floor(256 * (X - floor(X)))
  )
}

lat2y <- function(lat, zoom) {
  n <- 2 ^ zoom

  lat_rad <- lat * pi / 180
  Y <- (1 - (log(tan(lat_rad) + sec(lat_rad)) / pi)) / 2 * n

  data.frame(
    Y = floor(Y),
    y = floor(256 * (Y - floor(Y)))
  )
}


sec <- function(x) 1/cos(x)
