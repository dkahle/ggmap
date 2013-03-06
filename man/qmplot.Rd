\name{qmplot}
\alias{qmplot}
\title{Quick map plot}
\usage{
  qmplot(x, y, ..., data, zoom, source = "stamen",
    extent = "device", legend = "right", padding = 0.02,
    darken = c(0, "black"), mapcolor = "color",
    facets = NULL, margins = FALSE, geom = "auto",
    stat = list(NULL), position = list(NULL),
    xlim = c(NA, NA), ylim = c(NA, NA), main = NULL,
    f = 0.05, xlab = deparse(substitute(x)),
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

  \item{extent}{how much of the plot should the map take
  up? 'normal', 'panel', or 'device' (default)}

  \item{legend}{'left', 'right' (default), 'bottom', 'top',
  'bottomleft', 'bottomright', 'topleft', 'topright',
  'none' (used with extent = 'device')}

  \item{padding}{distance from legend to corner of the plot
  (used with extent = 'device')}

  \item{darken}{vector of the form c(number, color), where
  number is in [0, 1] and color is a character string
  indicating the color of the darken.  0 indicates no
  darkening, 1 indicates a black-out.}

  \item{mapcolor}{color ('color') or black-and-white
  ('bw')}

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


# only violent crimes
violent_crimes <- subset(crime,
  offense != 'auto theft' &
  offense != 'theft' &
  offense != 'burglary'
)

# rank violent crimes
violent_crimes$offense <-
  factor(violent_crimes$offense,
    levels = c('robbery', 'aggravated assault',
      'rape', 'murder')
  )

# restrict to downtown
violent_crimes <- subset(violent_crimes,
  -95.39681 <= lon & lon <= -95.34188 &
   29.73631 <= lat & lat <=  29.78400
)

theme_set(theme_bw())

qmplot(lon, lat, data = violent_crimes, colour = offense, darken = .5,
  size = I(3.5), alpha = I(.6), legend = 'topleft')

qmplot(lon, lat, data = violent_crimes, geom = c('point','density2d'))
qmplot(lon, lat, data = violent_crimes) + facet_wrap(~ offense)
qmplot(lon, lat, data = violent_crimes, extent = 'panel') + facet_wrap(~ offense)
qmplot(lon, lat, data = violent_crimes, extent = 'panel', colour = offense) +
  facet_wrap(~ month)




# doesn't quite work yet....
qmplot(long, lat, xend = long + delta_long,
  yend = lat + delta_lat, data = seals, geom = 'segment')


library(scales)
library(grid)
options('device')$device(width = 4.98, height = 5.97)
qmplot(lon, lat, data = wind, size = I(.5), alpha = I(.5)) +
  ggtitle('NOAA Wind Report Sites')

# thin down data set...
s <- seq(1, 227, 8)
thinwind <- subset(wind,
  lon \%in\% unique(wind$lon)[s] &
  lat \%in\% unique(wind$lat)[s]
)

# for some reason adding arrows to the following plot bugs
theme_set(theme_bw(18))
options('device')$device(width = 6.13, height = 7.04)
qmplot(lon, lat, data = thinwind, geom = 'tile', fill = spd, alpha = spd,
    legend = 'bottomleft') +
  geom_leg(aes(xend = lon + delta_lon, yend = lat + delta_lat)) +
  scale_fill_gradient2('Wind Speed\\nand\\nDirection',
    low = 'green', mid = muted('green'), high = 'red') +
  scale_alpha('Wind Speed\\nand\\nDirection', range = c(.1, .75)) +
  guides(fill = guide_legend(), alpha = guide_legend())




}
}

