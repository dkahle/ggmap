\name{get_stamenmap}
\alias{get_stamenmap}
\title{Get a Stamen Map}
\usage{
  get_stamenmap(bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344),
    zoom = 10,
    maptype = c("terrain", "watercolor", "toner"),
    crop = TRUE, messaging = FALSE, urlonly = FALSE,
    filename = "ggmapTemp", color = c("color", "bw"),
    checkargs = TRUE)
}
\arguments{
  \item{bbox}{a bounding box in the format c(lowerleftlon,
  lowerleftlat, upperrightlon, upperrightlat).}

  \item{zoom}{a zoom level}

  \item{maptype}{terrain, watercolor, or toner}

  \item{crop}{crop raw map tiles to specified bounding box}

  \item{messaging}{turn messaging on/off}

  \item{urlonly}{return url only}

  \item{filename}{destination file for download (file
  extension added according to format)}

  \item{color}{color or black-and-white}

  \item{checkargs}{check arguments}
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
  accesses stamen maps.
}
\examples{
\dontrun{

gc <- geocode('duncan hall, rice university')
google <- get_googlemap('rice university', zoom = 15)
ggmap(google) +
  geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 2)

bbox <- as.numeric(attr(google, 'bb'))[c(2,1,4,3)]
names(bbox) <- c('left','bottom','right','top')
stamen <- get_stamenmap(bbox, zoom = 15)
ggmap(stamen) +
  geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 2)

osm <- get_openstreetmap(bbox, scale = OSM_scale_lookup(15))
ggmap(osm) +
  geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 2)


ggmap(get_stamenmap(bbox, zoom = 15, maptype = 'watercolor'))+
  geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 2)

ggmap(get_stamenmap(bbox, zoom = 15, maptype = 'toner'))+
  geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 2)

}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \url{http://maps.stamen.com/#watercolor},
  \code{\link{ggmap}}
}

