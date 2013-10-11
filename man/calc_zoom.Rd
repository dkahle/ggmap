\name{calc_zoom}
\alias{calc_zoom}
\title{Calculate a zoom given a bounding box}
\usage{
  calc_zoom(lon, lat, data, adjust = 0, f = 0.05)
}
\arguments{
  \item{lon}{longitude, see details}

  \item{lat}{latitude, see details}

  \item{data}{(optional) a data frame containing lon and
  lat as variables}

  \item{adjust}{number to add to the calculated zoom}

  \item{f}{argument to pass to make_bbox}
}
\description{
  Calculate a zoom given a bounding box
}
\details{
  calc_zoom can calculate a zoom based on either (1) a data
  frame with longitude and latitude variables, (2) a
  longitude range and latitude range, or (3) a bounding
  box.  the specification for (1) is identical to that of
  most R functions, for (2) simply put in a longitude range
  into lon and a latitude range into lat, and for (3) put
  the bounding box in for the lon argument.
}
\examples{
\dontrun{

# data specification
calc_zoom(lon, lat, wind)


# range specification
calc_zoom(lon_range, lat_range)


# box specification
box <- make_bbox(lon, lat, data = crime)
calc_zoom(box)

}
}
\seealso{
  \code{\link{make_bbox}}
}

