\name{OSM_scale_lookup}
\alias{OSM_scale_lookup}
\title{Look up OpenStreetMap scale for a given zoom level.}
\usage{
  OSM_scale_lookup(zoom = 10)
}
\arguments{
  \item{zoom}{google zoom}
}
\value{
  scale
}
\description{
  Look up OpenStreetMap scale for a given zoom level.
}
\details{
  The calculation of an appropriate OSM scale value for a
  given zoom level is a complicated task.  For details, see
  \url{http://wiki.openstreetmap.org/wiki/FAQ} or
  \url{http://almien.co.uk/OSM/Tools/Scale/}.
}
\examples{
OSM_scale_lookup(zoom = 3)
OSM_scale_lookup(zoom = 10)

\dontrun{

ggmapplot(ggmap(zoom =  3, source = 'osm', scale = 47500000), fullpage = TRUE)
ggmapplot(ggmap(zoom =  4, source = 'osm', scale = 32500000), fullpage = TRUE)
ggmapplot(ggmap(zoom =  5, source = 'osm', scale = 15000000), fullpage = TRUE)
ggmapplot(ggmap(zoom =  6, source = 'osm', scale = 10000000), fullpage = TRUE)
ggmapplot(ggmap(zoom =  7, source = 'osm', scale =  6000000), fullpage = TRUE)
ggmapplot(ggmap(zoom =  8, source = 'osm', scale =  2800000), fullpage = TRUE)
ggmapplot(ggmap(zoom =  9, source = 'osm', scale =  1200000), fullpage = TRUE)
ggmapplot(ggmap(zoom = 10, source = 'osm', scale =   575000), fullpage = TRUE)
ggmapplot(ggmap(zoom = 11, source = 'osm', scale =   220000), fullpage = TRUE)
ggmapplot(ggmap(zoom = 12, source = 'osm', scale =   110000), fullpage = TRUE)
ggmapplot(ggmap(zoom = 13, source = 'osm', scale =    70000), fullpage = TRUE)
ggmapplot(ggmap(zoom = 14, source = 'osm', scale =    31000), fullpage = TRUE)
ggmapplot(ggmap(zoom = 15, source = 'osm', scale =    15000), fullpage = TRUE)
ggmapplot(ggmap(zoom = 16, source = 'osm', scale =     7500), fullpage = TRUE)
ggmapplot(ggmap(zoom = 17, source = 'osm', scale =     4000), fullpage = TRUE)
ggmapplot(ggmap(zoom = 18, source = 'osm', scale =     2500), fullpage = TRUE)
ggmapplot(ggmap(zoom = 19, source = 'osm', scale =     1750), fullpage = TRUE)
ggmapplot(ggmap(zoom = 20, source = 'osm', scale =     1000), fullpage = TRUE)

# the USA
lonR <- c(1.01,.99)*c(-124.73,-66.95)
latR <- c(.99,1.01)*c(24.52, 49.38)
qmap(lonR = lonR, latR = latR, source = 'osm', scale = 325E5)

}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}

