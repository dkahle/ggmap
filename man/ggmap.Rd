\docType{package}
\name{ggmap}
\alias{ggmap}
\alias{ggmap-package}
\alias{package-ggmap}
\title{Grab a map.}
\usage{
  ggmap(location = c(lon = -95.3632715, lat = 29.7632836),
    zoom = 10, scale = "auto",
    maptype = c("terrain", "satellite", "roadmap", "hybrid", "toner", "watercolor"),
    messaging = FALSE, urlonly = FALSE,
    filename = "ggmapTemp", crop = TRUE,
    color = c("color", "bw"),
    source = c("google", "osm", "stamen"))
}
\arguments{
  \item{location}{either be an address, longitude/latitude
  pair, or left/bottom/right/top bounding box}

  \item{zoom}{map zoom, an integer from 0 (whole world) to
  21 (building), default value 10 (city).  openstreetmaps
  limits a zoom of 18, and the limit on stamen maps depends
  on the maptype}

  \item{scale}{scale, see \code{\link{get_openstreetmap}}}

  \item{maptype}{character string providing map theme.
  options available are 'terrain', 'satellite', 'roadmap',
  and 'hybrid' (google maps) or 'terrain', 'watercolor',
  and 'toner' (stamen maps)}

  \item{source}{Google Maps ('google'), OpenStreetMap
  ('osm'), Stamen Maps ('stamen')}

  \item{messaging}{turn messaging on/off}

  \item{urlonly}{return url only}

  \item{filename}{destination file for download (file
  extension added according to format)}

  \item{crop}{(stamen maps) crop tiles to bounding box}

  \item{color}{color or black-and-white}
}
\value{
  a data.frame with columns latitude, longitude, and fill
}
\description{
  ggmap is a smart function which queries the Google Maps,
  OpenStreetMap, or Stamen Maps server for a map at a
  certain location at a certain spatial zoom.  it is a
  wrapper for get_googlemap, get_openstreetmap, and
  get_stamenmap functions.

  ggmap integrates ggplot2 and RgoogleMaps to create an
  elegant, consistent, and modular framework for spatial
  graphics. More information about each function can be
  found in its help documentation.
}
\examples{
\dontrun{
map <- ggmap()
ggmapplot(map, fullpage = TRUE)

map <- ggmap(maptype = 'roadmap')
ggmapplot(map, fullpage = TRUE)

map <- ggmap(maptype = 'hybrid')
ggmapplot(map, fullpage = TRUE)

map <- ggmap(maptype = 'satellite')
ggmapplot(map, fullpage = TRUE)

map <- ggmap(source = 'osm')
ggmapplot(map, fullpage = TRUE)

map <- ggmap(source = 'stamen')
ggmapplot(map, fullpage = TRUE)

map <- ggmap(source = 'stamen', maptype = 'watercolor')
ggmapplot(map, fullpage = TRUE)

map <- ggmap(source = 'stamen', maptype = 'toner')
ggmapplot(map, fullpage = TRUE)



map <- ggmap(location = 'texas', zoom = 6, source = 'stamen')
ggmapplot(map, fullpage = TRUE)

map <- ggmap(location = 'united states', zoom = 4, source = 'stamen')
ggmapplot(map, fullpage = TRUE)


}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \code{\link{ggmapplot}}, \code{\link{GetMap}} in package
  RgoogleMaps
}

