\name{ggmap}
\alias{ggmap}
\title{Grab a map.}
\usage{
  ggmap(location = "houston", lonR, latR,
    center = c(lat = 29.7632836, lon = -95.3632715),
    regularize = TRUE, type = c("color", "bw"),
    rgbcoefs = c(0, 1, 0), zoom = 10, maptype = "terrain",
    source = c("google", "osm"), verbose = FALSE,
    destfile = "ggmapTemp.jpg", n_pix = 640,
    scale = OSM_scale_lookup(zoom), raster = TRUE, ...)
}
\arguments{
  \item{location}{a character string containing the name of
  the location of interest}

  \item{lonR}{longitude range (only for OpenStreetMaps)}

  \item{latR}{latitude range (only for OpenStreetMaps)}

  \item{center}{named numeric vector of latitude and
  longitude specifying the center of the image}

  \item{regularize}{logical; should the map grid be
  regularized?}

  \item{type}{'color' for a color map, 'bw' for a black and
  white map}

  \item{rgbcoefs}{when 'bw' is specified, conversions
  coefficients to use.  see ?ReadImages::rgb2grey}

  \item{zoom}{level of zoom, an integer 0 (whole world) to
  19 (higest zoom), default value 10}

  \item{maptype}{any of 'roadmap', 'mobile', 'satellite',
  'terrain', 'hybrid', 'mapmaker-roadmap',
  'mapmaker-hybrid'}

  \item{source}{'google' or 'osm' (OpenStreetMaps)}

  \item{verbose}{logical; should function message user?}

  \item{destfile}{character; name of file to save
  downloaded map}

  \item{n_pix}{numeric; number of pixels in map}

  \item{scale}{numeric; scale of OpenStreetMap, see
  ?GetMap}

  \item{raster}{logical; use geom_raster?}

  \item{...}{...}
}
\value{
  a data.frame with columns latitude, longitude, and fill
}
\description{
  ggmap is a smart function which queries the Google Maps
  server or OpenStreetMap server for a map at a certain
  location at a certain spatial zoom.
}
\examples{
\dontrun{
WashingtonMap_df <- ggmap(location = 'washington', verbose = TRUE)
str(WashingtonMap_df)
ggmapplot(WashingtonMap_df)

lonR <- c(-97.12008, -97.11836)
latR <- c(31.54765, 31.54911)
osm <- ggmap(latR = latR, lonR = lonR, source = 'osm', scale = 1000)
ggmapplot(osm)

}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \code{\link{ggmapplot}}, \code{\link{GetMap}} in package
  RgoogleMaps
}

