\name{geom_raster}
\alias{geom_raster}
\title{geom-raster}
\usage{
  geom_raster(mapping = NULL, data = NULL,
    stat = "identity", position = "identity", image = NULL,
    interpolate = TRUE, ...)
}
\arguments{
  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults.}

  \item{stat}{The statistical transformation to use on the
  data for this layer.}

  \item{position}{The position adjustment to use for
  overlappling points on this layer}

  \item{image}{raster image}

  \item{interpolate}{whether to interpolate.  see
  ?grid::rasterGrob}

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}
}
\value{
  a named numeric vector of geographical coordinates.
}
\description{
  a special version of geom_raster (ggplot2) for use in
  ggmap.
}
\examples{
NULL
}
\author{
  Hadley Wickham with additions by Kohske Takahashi
}

