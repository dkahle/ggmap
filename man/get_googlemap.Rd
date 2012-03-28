\name{get_googlemap}
\alias{get_googlemap}
\title{Get a Google Map}
\usage{
  get_googlemap(center = c(lon = -95.3632715, lat = 29.7632836),
    zoom = 10, size = c(640, 640), scale = 2,
    format = c("png8", "gif", "jpg", "jpg-baseline", "png32"),
    maptype = c("terrain", "satellite", "roadmap", "hybrid"),
    language = "en-EN", region, markers, path, visible,
    style, sensor = FALSE, messaging = FALSE,
    urlonly = FALSE, filename = "ggmapTemp",
    color = c("color", "bw"), checkargs = TRUE)
}
\arguments{
  \item{center}{the center of the map.  this can either be
  1. a longitude/latitude numeric vector or 2. a character
  string address (note that the latter uses a geocode)}

  \item{zoom}{map zoom, an integer from 0 (whole world) to
  21 (building), default value 10 (city)}

  \item{size}{rectangular dimensions of map in pixels -
  horizontal x vertical - with a max of c(640, 640).  this
  parameter is affected in a multiplicative way by scale.}

  \item{scale}{multiplicative factor for the number of
  pixels returned possible values are 1, 2, or 4 (e.g. size
  = c(640,640) and scale = 2 returns an image with
  1280x1280 pixels).  4 is reserved for google business
  users only.  scale also affects the size of labels as
  well.}

  \item{format}{character string providing image format -
  png, jpeg, and gif formats available in various flavors}

  \item{maptype}{character string providing google map
  theme. options available are 'terrain', 'satellite',
  'roadmap', and 'hybrid'}

  \item{language}{character string providing language of
  map labels (for themes with them) in the format 'en-EN'.
  not all languages are supported; for those which aren't
  the default language is used}

  \item{region}{borders to display as a region code
  specified as a two-character ccTLD ('top-level domain')
  value, see
  \url{http://en.wikipedia.org/wiki/List_of_Internet_top-level_domains#Country_code_top-level_domains}}

  \item{markers}{data.frame with first column longitude,
  second column latitude, for which google markers should
  be embedded in the map image, or character string to be
  passed directly to api}

  \item{path}{data.frame (or list of data.frames) with
  first column longitude, second column latitude, for which
  a single path should be embedded in the map image, or
  character string to be passed directly to api}

  \item{visible}{a location as a longitude/latitude numeric
  vector (or data frame with first column longitude, second
  latitude) or vector of character string addresses which
  should be visible in map extent}

  \item{style}{character string to be supplied directly to
  the api for the style argument .  this is a powerful
  complex specification, see
  \url{https://developers.google.com/maps/documentation/staticmaps/}}

  \item{sensor}{specifies whether the application
  requesting the static map is using a sensor to determine
  the user's location}

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
  get_googlemap accesses the Google Static Maps API version
  2 to download a static map.
}
\examples{
\dontrun{

get_googlemap(urlonly = TRUE)

# get_googlemap has several argument checks
get_googlemap(zoom = 13.5)
get_googlemap(scale = 3)
get_googlemap(center = c(-30,-110))

# markers and paths are easy to access
d <- function(x=-95.36, y=29.76, n,r,a){
  round(data.frame(
    lon = jitter(rep(x,n), amount = a),
    lat = jitter(rep(y,n), amount = a)
  ), digits = r)
}
df <- d(n=50,r=3,a=.3)
map <- get_googlemap(markers = df, path = df,, scale = 2)
ggmap(map)
ggmap(map, fullpage = TRUE) +
  geom_point(aes(x = lon, y = lat), data = df, size = 3, colour = 'black') +
  geom_path(aes(x = lon, y = lat), data = df)

gc <- geocode('waco, texas')
center <- as.numeric(gc)
ggmap(get_googlemap(center = center, color = 'bw', scale = 2), fullpage = T)

# the scale argument can be seen in the following
# (make your graphics device as large as possible)
ggmap(get_googlemap(center, scale = 1), fullpage = TRUE) # pixelated
ggmap(get_googlemap(center, scale = 2), fullpage = TRUE) # fine


}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \url{https://developers.google.com/maps/documentation/staticmaps/},
  \code{\link{ggmap}}
}

