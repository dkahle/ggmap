\name{qmap}
\alias{qmap}
\title{Quick map plot}
\usage{
  qmap(location, ...)
}
\arguments{
  \item{location}{character; location of interest}

  \item{...}{stuff to pass to \code{\link{ggmapplot}} and
  \code{\link{ggmap}}.}
}
\value{
  a ggplot object
}
\description{
  qmap is a wrapper for \code{\link{ggmapplot}} and
  \code{\link{ggmap}}.
}
\examples{
\dontrun{
qmap(location = 'waco')
qmap(location = 'waco', zoom = 14)
qmap(location = 'waco', zoom = 14, source = 'osm')
qmap(location = 'waco', zoom = 14, source = 'osm', scale = 20000)
qmap(location = 'waco', zoom = 14, maptype = 'satellite')
qmap(location = 'waco', zoom = 14, maptype = 'hybrid')

wh <- geocode('the white house')
qmap('the white house', base_layer = ggplot(aes(x=lon, y=lat), data = wh)) +
  geom_point()
qmap('the white house', maprange = TRUE,
  base_layer = ggplot(aes(x=lon, y=lat), data = wh)) +
  geom_point()




}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \code{\link{ggmapplot}} and \code{\link{ggmap}}.
}

