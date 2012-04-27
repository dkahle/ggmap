\name{get_map}
\alias{get_map}
\title{Grab a map.}
\usage{
  get_map(location = c(lon = -95.3632715, lat = 29.7632836),
    zoom = "auto", scale = "auto",
    maptype = c("terrain", "satellite", "roadmap", "hybrid", "toner", "watercolor"),
    messaging = FALSE, urlonly = FALSE,
    filename = "ggmapTemp", crop = TRUE,
    color = c("color", "bw"),
    source = c("google", "osm", "stamen", "cloudmade"),
    api_key)
}
\arguments{
  \item{location}{an address, longitude/latitude pair (in
  that order), or left/bottom/right/top bounding box}

  \item{zoom}{map zoom, an integer from 0 (whole world) to
  21 (building), default value 10 (city).  openstreetmaps
  limits a zoom of 18, and the limit on stamen maps depends
  on the maptype.  'auto' automatically determines the zoom
  for bounding box specifications, and is defaulted to 10
  with center/zoom specifications.}

  \item{scale}{scale argument of
  \code{\link{get_googlemap}} or
  \code{\link{get_openstreetmap}}}

  \item{maptype}{character string providing map theme.
  options available are 'terrain', 'satellite', 'roadmap',
  and 'hybrid' (google maps), 'terrain', 'watercolor', and
  'toner' (stamen maps), or a positive integer for
  cloudmade maps (see ?get_cloudmademap)}

  \item{source}{Google Maps ('google'), OpenStreetMap
  ('osm'), Stamen Maps ('stamen'), or CloudMade maps
  ('cloudmade')}

  \item{messaging}{turn messaging on/off}

  \item{urlonly}{return url only}

  \item{filename}{destination file for download (file
  extension added according to format)}

  \item{crop}{(stamen and cloudmade maps) crop tiles to
  bounding box}

  \item{color}{color ('color') or black-and-white ('bw')}

  \item{api_key}{an api key for cloudmade maps}
}
\value{
  a data.frame with columns latitude, longitude, and fill
}
\description{
  get_map is a smart function which queries the Google
  Maps, OpenStreetMap, or Stamen Maps server for a map at a
  certain location at a certain spatial zoom.  it is a
  wrapper for get_googlemap, get_openstreetmap,
  get_stamenmap, and get_cloudmademap functions.  get_map
  was formerly (<2.0) called ggmap.
}
\examples{
\dontrun{
map <- get_map()
ggmap(map, fullpage = TRUE)

map <- get_map(maptype = 'roadmap')
ggmap(map, fullpage = TRUE)

map <- get_map(maptype = 'hybrid')
ggmap(map, fullpage = TRUE)

map <- get_map(maptype = 'satellite')
ggmap(map, fullpage = TRUE)

map <- get_map(source = 'osm')
ggmap(map, fullpage = TRUE)

map <- get_map(source = 'stamen')
ggmap(map, fullpage = TRUE)

map <- get_map(source = 'stamen', maptype = 'watercolor')
ggmap(map, fullpage = TRUE)

map <- get_map(source = 'stamen', maptype = 'toner')
ggmap(map, fullpage = TRUE)



map <- get_map(location = 'texas', zoom = 6, source = 'stamen')
ggmap(map, fullpage = TRUE)

map <- get_map(location = 'united states', zoom = 4, source = 'stamen')
ggmap(map, fullpage = TRUE)

api_key <- '<your api key here>'
map <- get_map(location = 'baylor university', source = 'cloudmade',
  maptype = 53428, api_key = api_key, zoom = 14)
ggmap(map, fullpage = TRUE)


}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \code{\link{ggmap}}, \code{\link{GetMap}} in package
  RgoogleMaps
}

