\name{mapdist}
\alias{mapdist}
\title{Compute map distances using Google}
\usage{
  mapdist(from, to,
    mode = c("driving", "walking", "bicycling"),
    output = c("simple", "all"), messaging = FALSE,
    sensor = FALSE, language = "en-EN",
    override_limit = FALSE)
}
\arguments{
  \item{from}{name of origin addresses in a data frame
  (vector accepted)}

  \item{to}{name of destination addresses in a data frame
  (vector accepted)}

  \item{output}{amount of output}

  \item{mode}{driving, bicycling, or walking}

  \item{messaging}{turn messaging on/off}

  \item{sensor}{whether or not the geocoding request comes
  from a device with a location sensor}

  \item{language}{language}

  \item{override_limit}{override the current query count
  (.GoogleDistQueryCount)}
}
\value{
  a data frame (output='simple') or all of the geocoded
  information (output='all')
}
\description{
  Compute map distances using Google Maps
}
\details{
  if parameters from and to are specified as geographic
  coordinates, they are reverse geocoded with revgeocode.
  note that the google maps api limits to 2500 element
  queries a day.
}
\examples{
\dontrun{

from <- c('houston, texas', 'dallas')
to <- 'waco, texas'
mapdist(from, to)
mapdist(from, to, mode = 'bicycling')
mapdist(from, to, mode = 'walking')

from <- c('houston', 'houston', 'dallas')
to <- c('waco, texas', 'san antonio', 'houston')
mapdist(from, to)

mapdist('the white house', 'washington monument', mode = 'walking')

# geographic coordinates are accepted as well
(wh <- as.numeric(geocode('the white house')))
(wm <- as.numeric(geocode('washington monument')))
mapdist(wh, wm, mode = 'walking')
mapdist('the white house', wm, mode = 'walking')
distQueryCheck()

}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \url{http://code.google.com/apis/maps/documentation/distancematrix/}
}

