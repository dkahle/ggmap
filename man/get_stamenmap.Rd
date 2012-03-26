\name{get_stamenmap}
\alias{get_stamenmap}
\title{Get a Stamen Map.}
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

gc <- geocode('rice university')
stamen <- get_stamenmap(crop = TRUE)
ggmapplot(stamen) +
  geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 3)

osm <- get_openstreetmap(checkargs = FALSE)
ggmapplot(osm) +
  geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 3)

google <- get_googlemap(checkargs = FALSE)
ggmapplot(google) +
  geom_point(aes(x = lon, y = lat), data = gc, colour = 'red', size = 3)

ggmapplot(get_stamenmap(maptype = 'watercolor'))
ggmapplot(get_stamenmap(maptype = 'watercolor', color = 'bw'))

ggmapplot(get_stamenmap(maptype = 'toner'))

}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \url{http://maps.stamen.com/#watercolor},
  \code{\link{ggmapplot}}
}

