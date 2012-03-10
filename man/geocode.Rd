\name{geocode}
\alias{geocode}
\title{Geocode}
\usage{
  geocode(location,
    output = c("latlon", "latlona", "more", "all"),
    messaging = FALSE)
}
\arguments{
  \item{location}{a character string specifying a location
  of interest (e.g. "Baylor University")}

  \item{output}{amount of output}

  \item{messaging}{turn messaging on/off}
}
\value{
  depends (at least a data.frame with variables lon and
  lat)
}
\description{
  geocodes a location using Google Maps.
}
\details{
  note that the google maps api limits to 2500 queries a
  day.
}
\examples{
\dontrun{
geocode('Baylor University')
geocode('1600 Pennsylvania Avenue, Washington DC')
geocode('1600 Pennsylvania Avenue, Washington DC', messaging = TRUE)
geocode('the white house', messaging = TRUE)
geocode('the eiffel tower')
geocode(c('baylor university', 'salvation army waco', 'HEB #087 waco'))
geocode(c('baylor university', 'the vatican'))
geocode(c('baylor university', 'the vatican'), output = 'latlona')
geocode(c('baylor university', 'the vatican'), output = 'more')
geocode('the eiffel tower', output = 'all')
}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \url{http://code.google.com/apis/maps/documentation/geocoding/}
}

