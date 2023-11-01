# these functions were brought in essentially verbatim from
# RgoogleMaps to avoid the problems of that package's depencency
# on the deprecated sp package.


XY2LatLon <- function (MyMap, X, Y, zoom) {
  if (!missing(MyMap)) {
    lat.center <- MyMap[[1]]
    lon.center <- MyMap[[2]]
    if (missing(zoom))
      zoom <- MyMap[[3]]
    mycenter <- LatLon2XY(lat.center, lon.center, zoom)
    x <- mycenter$Tile[, "X"] + (X + mycenter$Coords[, "x"])/256
    y <- mycenter$Tile[, "Y"] - (Y - mycenter$Coords[, "y"])/256
  }
  else {
    x = X
    y = Y
  }
  ytilde <- 1 - y/2^(zoom - 1)
  yy = (exp(2 * pi * ytilde) - 1)/(exp(2 * pi * ytilde) + 1)
  ShiftLat <- function(yy) {
    n = c(-1, 0, 1)
    lat = 2 * pi * (n) + asin(yy)
    lat <- lat[which(lat <= pi/2 & lat > -pi/2)]
    lat <- 180 * lat/pi
    return(lat)
  }
  lat <- sapply(yy, ShiftLat)
  lon <- 180 * (x/2^(zoom - 1) - 1)
  cbind(lat = lat, lon = lon)
}




LatLon2XY <- function (lat, lon, zoom) {
  SinPhi = sin(lat * pi/180)
  normX = lon/180
  normY = (0.5 * log((1 + SinPhi)/(1 - SinPhi)))/pi
  Y <- (2^zoom) * ((1 - normY)/2)
  X <- (2^zoom) * ((normX + 1)/2)
  x <- 256 * (X - floor(X))
  y <- 256 * (Y - floor(Y))

  list(
    Tile = cbind(X = floor(X), Y = floor(Y)),
    Coords = cbind(x = x, y = y)
  )
}
