\name{qmplot}
\alias{qmplot}
\title{Quick map plot}
\usage{
  qmplot(x, y, ..., data, zoom, source = "stamen",
    fullpage = TRUE, facets = NULL, margins = FALSE,
    geom = "auto", stat = list(NULL),
    position = list(NULL), xlim = c(NA, NA),
    ylim = c(NA, NA), main = NULL, f = 0.05,
    xlab = deparse(substitute(x)),
    ylab = deparse(substitute(y)))
}
\arguments{
  \item{x}{longitude values}

  \item{y}{latitude values}

  \item{...}{other aesthetics passed for each layer}

  \item{data}{data frame to use (optional).  If not
  specified, will create one, extracting vectors from the
  current environment.}

  \item{zoom}{map zoom, see \code{\link{get_map}}}

  \item{source}{map source, see \code{\link{get_map}}}

  \item{fullpage}{should the map fill the device}

  \item{facets}{faceting formula to use.  Picks
  \code{\link{facet_wrap}} or \code{\link{facet_grid}}
  depending on whether the formula is one sided or
  two-sided}

  \item{margins}{whether or not margins will be displayed}

  \item{geom}{character vector specifying geom to use.
  defaults to "point"}

  \item{stat}{character vector specifying statistics to
  use}

  \item{position}{character vector giving position
  adjustment to use}

  \item{xlim}{limits for x axis}

  \item{ylim}{limits for y axis}

  \item{main}{character vector or expression for plot
  title}

  \item{f}{number specifying the fraction by which the
  range should be extended}

  \item{xlab}{character vector or expression for x axis
  label}

  \item{ylab}{character vector or expression for y axis
  label}
}
\description{
  \code{qmplot} is the ggmap equivalent to the ggplot2
  function qplot and allows for the quick plotting of maps
  with data/models/etc.  qmplot is still experimental.
}
\examples{
\donttest{
qmplot(lon, lat, data = crime)
qmplot(lon, lat, data = crime, colour = offense)
qmplot(lon, lat, data = crime, geom = c('point','density2d'))
qmplot(lon, lat, data = crime) + facet_wrap(~ offense)
qmplot(lon, lat, data = crime, fullpage = FALSE) + facet_wrap(~ offense)
qmplot(lon, lat, data = crime, fullpage = FALSE, colour = offense) +
  facet_wrap(~ month)
qmplot(long, lat, xend = long + delta_long,
  yend = lat + delta_lat, data = seals, geom = 'segment')


library(scales)
library(grid)
qmplot(lon, lat, data = wind, size = I(.5), alpha = I(.5))

# thin down data set...
s <- seq(1, 227, 8)
thinwind <- subset(wind,
  lon \%in\% unique(wind$lon)[s] &
  lat \%in\% unique(wind$lat)[s]
)

# for some reason adding arrows to the following plot bugs
qmplot(lon, lat, data = thinwind, geom = 'tile', fill = spd, alpha = spd) +
  geom_leg(aes(xend = lon + delta_lon, yend = lat + delta_lat)) +
  scale_fill_gradient2(low = 'green', mid = muted('green'), high = 'red') +
  scale_alpha(range = c(.1, .75))




}
}

