\name{get_stamenmap}
\alias{get_stamenmap}
\title{Get a Stamen Map}
\usage{
  get_stamenmap(bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344),
    zoom = 10,
    maptype = c("terrain", "watercolor", "toner"),
    crop = TRUE, messaging = FALSE, urlonly = FALSE,
    color = c("color", "bw"), force = FALSE, ...)
}
\arguments{
  \item{bbox}{a bounding box in the format c(lowerleftlon,
  lowerleftlat, upperrightlon, upperrightlat).}

  \item{zoom}{a zoom level}

  \item{maptype}{terrain, watercolor, or toner}

  \item{crop}{crop raw map tiles to specified bounding box}

  \item{messaging}{turn messaging on/off}

  \item{urlonly}{return url only}

  \item{color}{color or black-and-white}

  \item{force}{if the map is on file, should a new map be
  looked up?}

  \item{...}{...}
}
\value{
  a map image as a 2d-array of colors as hexadecimal
  strings representing pixel fill values.
}
\description{
  get_stamenmap accesses a tile server for Stamen Maps and
  downloads/stiches map tiles/formats a map image.
}
\details{
  Note that Stamen maps don't cover the entire world.  For
  example, see
  http://tile.stamen.com/terrain/#4/30.28/-87.21

  accesses stamen maps.
}
\examples{
\dontrun{

gc <- geocode("marrs mclean science building, baylor university")
google <- get_googlemap("baylor university", zoom = 15)
ggmap(google) +
  geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)

bbox <- c(left = -97.132, bottom = 31.536, right = -97.105, top = 31.560)
ggmap(get_stamenmap(bbox, zoom = 13))
ggmap(get_stamenmap(bbox, zoom = 14))
ggmap(get_stamenmap(bbox, zoom = 15))
# ggmap(get_stamenmap(bbox, zoom = 16))
# ggmap(get_stamenmap(bbox, zoom = 17))

ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 11), extent = "device")
ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 12), extent = "device")
ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 13), extent = "device")
ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 14), extent = "device")
ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 15), extent = "device")
# ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 16), extent = "device")
# ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 17), extent = "device")
# ggmap(get_stamenmap(bbox, maptype = "watercolor", zoom = 18), extent = "device")

stamen <- get_stamenmap(bbox, zoom = 15)
ggmap(stamen) +
  geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)

stamen <- get_stamenmap(bbox, zoom = 15, crop = FALSE)
ggmap(stamen) +
  geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)

osm <- get_openstreetmap(bbox, scale = OSM_scale_lookup(15))
ggmap(osm) +
  geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)


ggmap(get_stamenmap(bbox, zoom = 15, maptype = "watercolor"))+
  geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)

ggmap(get_stamenmap(bbox, zoom = 15, maptype = "toner"))+
  geom_point(aes(x = lon, y = lat), data = gc, colour = "red", size = 2)

}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \url{http://maps.stamen.com/#watercolor},
  \code{\link{ggmap}}
}

