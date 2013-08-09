\name{get_navermap}
\alias{get_navermap}
\title{Get a Naver Map}
\usage{
  get_navermap(crs = "EPSG:4326",
    center = c(lon = 126.9849208, lat = 37.5664519),
    size = c(640, 640), format = c("png", "jpeg", "jpg"),
    color = c("color", "bw"), level = 4,
    baselayer = c("default", "satellite"),
    overlayers = c("anno_satellite", "bicycle", "roadview", "traffic"),
    key, uri, markers, filename = "", messaging = FALSE,
    urlonly = FALSE, force = FALSE, ...)
}
\arguments{
  \item{crs}{Coordinate system, this currently support
  EPSG:4326}

  \item{center}{the center of the map.  this can be
  longitude/latitude numeric vector.}

  \item{level}{map zoom, an integer from 1 to 14
  (building), default value 10}

  \item{size}{rectangular dimensions of map in pixels -
  horizontal x vertical - with a max of c(640, 640).}

  \item{format}{character string providing image format -
  png, jpeg(jpg) formats available in various flavors}

  \item{baselayer}{base layer, this can be either
  `default', `satellite'.}

  \item{overlayers}{overlay layers, this can be
  `anno_satellite',`bicycle', `roadview', `traffic'.}

  \item{markers}{data.frame with first column longitude,
  second column latitude, for which naver markers should be
  embedded in the map image, or character string to be
  passed directly to api}

  \item{messaging}{turn messaging on/off}

  \item{urlonly}{return url only}

  \item{key}{key code from naver api center}

  \item{uri}{registered host url}

  \item{color}{color or black-and-white}

  \item{force}{if the map is on file, should a new map be
  looked up?w}

  \item{filename}{destination file for download (file
  extension added according to format); deprecated}

  \item{...}{...}
}
\description{
  get_navermap accesses the Naver Static Maps API version
  1.1 to download a static map.  Note that in most cases by
  using this function you are agreeing to the Naver Maps
  API Terms of Service at
  http://dev.naver.com/openapi/apis/map/staticmap.
}
\author{
  Heewon Jeon \email{madjakarta@gmail.com}
}
\seealso{
  \url{http://dev.naver.com/openapi/apis/map/staticmap/},
  \code{\link{ggmap}}
}

