\name{inset_raster}
\alias{inset_raster}
\title{Add a raster annotation to a map}
\usage{
  inset_raster(raster, xmin, xmax, ymin, ymax)
}
\arguments{
  \item{raster}{raster object to display}

  \item{xmin,xmax}{x location (in data coordinates) giving
  horizontal location of raster}

  \item{ymin,ymax}{y location (in data coordinates) giving
  vertical location of raster}
}
\description{
  This is a special version of ggplot2::annotation_raster
  for use with ggmap
}
\details{
  Most useful for adding bitmap images
}
