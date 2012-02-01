\name{geocode}
\alias{geocode}
\title{Geocode}
\usage{
  geocode(location)
}
\arguments{
  \item{location}{a character string specifying a location
  of interest (e.g. "Baylor University")}
}
\value{
  a named numeric vector of geographical coordinates.
}
\description{
  geocodes a location using google maps.
}
\examples{
\dontrun{
geocode('Baylor University')
geocode('1600 Pennsylvania Avenue, Washington DC')
geocode('the eiffel tower')
}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}

