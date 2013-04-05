\name{geocode}
\alias{geocode}
\title{Geocode}
\usage{
  geocode(location,
    output = c("latlon", "latlona", "more", "all"),
    messaging = FALSE, sensor = FALSE,
    override_limit = FALSE)
}
\arguments{
  \item{location}{a character string specifying a location
  of interest (e.g. "Baylor University")}

  \item{output}{amount of output}

  \item{messaging}{turn messaging on/off}

  \item{sensor}{whether or not the geocoding request comes
  from a device with a location sensor}

  \item{override_limit}{override the current query count
  (.GoogleGeocodeQueryCount)}
}
\value{
  depends (at least a data.frame with variables lon and
  lat)
}
\description{
  geocodes a location using Google Maps.  Note that in most
  cases by using this function you are agreeing to the
  Google Maps API Terms of Service at
  https://developers.google.com/maps/terms.
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
geocode(c('baylor university', 'salvation army waco'))
geocode(c('baylor university', 'the vatican'))
geocode(c('baylor university', 'the vatican'), output = 'latlona')
geocode(c('baylor university', 'the vatican'), output = 'more')
geocode('the eiffel tower', output = 'all')
geocodeQueryCheck()

# careful in running this...
library(stringr)
ads <- unique(crime$address)[1:120]
ads <- paste(ads, ', houston, texas', sep = '')
ads <- str_trim(ads)
gc <- geocode(ads)
}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \url{http://code.google.com/apis/maps/documentation/geocoding/}
}

