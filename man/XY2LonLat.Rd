\name{XY2LonLat}
\alias{XY2LonLat}
\title{Convert a tile coordinate to a lon/lat coordinate}
\usage{
  XY2LonLat(X, Y, zoom, x = 0, y = 0, xpix = 256,
    ypix = 256)
}
\arguments{
  \item{X}{horizontal map-tile coordinate (0 is map-left)}

  \item{Y}{vertical map-tile coordinate (0 is map-top)}

  \item{zoom}{zoom}

  \item{x}{within tile x (0 is tile-left)}

  \item{y}{within tile y (0 it tile-top)}

  \item{xpix}{width of tile in pixels}

  \item{ypix}{length of tile in pixels}
}
\value{
  a data frame with columns lon and lat (in degrees)
}
\description{
  Convert a tile coordinate to a lon/lat coordinate for a
  given zoom.  Decimal tile coordinates are accepted.
}
\examples{
\dontrun{
XY2LonLat(480, 845, zoom = 11)

}
}
\author{
  David Kahle \email{david.kahle@gmail.com}, based on
  function XY2LatLon by Markus Loecher, Sense Networks
  \email{markus@sensenetworks.com} in package RgoogleMaps
}
\seealso{
  \url{http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames}
}

