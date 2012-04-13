\docType{package}
\name{ggmap}
\alias{ggmap}
\alias{ggmap-package}
\alias{package-ggmap}
\title{Plot a ggmap object}
\usage{
  ggmap(ggmap, fullpage = FALSE, base_layer,
    maprange = FALSE, expand = FALSE, legend = "topleft",
    b = 0.02, ...)
}
\arguments{
  \item{ggmap}{an object of class ggmap (from function
  get_map)}

  \item{fullpage}{logical; should the map take up the
  entire viewport?}

  \item{base_layer}{a ggplot(aes(...), ...) call; see
  examples}

  \item{maprange}{logical for use with base_layer; should
  the map define the x and y limits?}

  \item{expand}{should the map extend to the edge of the
  panel? used with base_layer and maprange=TRUE.}

  \item{legend}{'bottomleft', 'bottomright', 'topleft',
  'topright', 'none' (used with fullpage)}

  \item{b}{distance from legend to corner of the plot (used
  with legend)}

  \item{...}{...}
}
\value{
  a ggplot object
}
\description{
  ggmap plots the raster object produced by
  \code{\link{get_map}}.
}
\examples{
\dontrun{
hdf <- get_map()
(HoustonMap <- ggmap(hdf))

require(MASS)
mu <- c(-95.3632715, 29.7632836); nDataSets <- sample(4:10,1)
chkpts <- NULL
for(k in 1:nDataSets){
  a <- rnorm(2); b <- rnorm(2); si <- 1/3000 * (outer(a,a) + outer(b,b))
  chkpts <- rbind(chkpts, cbind(mvrnorm(rpois(1,50), jitter(mu, .01), si), k))
}
chkpts <- data.frame(chkpts)
names(chkpts) <- c('lon', 'lat','class')
chkpts$class <- factor(chkpts$class)
qplot(lon, lat, data = chkpts, colour = class)

HoustonMap +
  geom_point(aes(x = lon, y = lat, colour = class), data = chkpts, alpha = .5)


HoustonMap <- ggmap(get_map(maptype = 'satellite'), fullpage = TRUE)
HoustonMap +
  stat_density2d(aes(x = lon, y = lat, colour = class), data = chkpts, bins = 5)

ggmap(get_map('Paris', messaging = TRUE), fullpage = TRUE)


library(grid)
baylor <- get_map('baylor university', zoom = 15,
  maptype = 'satellite', messaging = TRUE)

ggmap(baylor) + theme_bw() +
  annotate('rect', xmin=-97.11920, ymin=31.5439, xmax=-97.101, ymax=31.5452,
    fill = I('black'), alpha = I(3/4)) +
  annotate('segment', x=-97.110, xend=-97.11920, y=31.5450, yend=31.5482,
    colour=I('red'), arrow = arrow(length=unit(0.3,"cm")), size = 1.5) +
  annotate('text', x=-97.110, y=31.5445, label = 'Department of Statistical Science',
    colour = I('red'), size = 6) +
  labs(x = 'Longitude', y = 'Latitude') + opts(title = 'Baylor University')

# the following is helpful to place the annotation boxes
clicks <- gglocator(2)
expand.grid(lon = clicks$lon, lat = clicks$lat)


baylor <- get_map('baylor university', zoom = 16,
  maptype = 'satellite', messaging = TRUE)

ggmap(baylor, fullpage = TRUE) +
  annotate('rect', xmin=-97.1164, ymin=31.5441, xmax=-97.1087, ymax=31.5449,
    fill = I('black'), alpha = I(3/4)) +
  annotate('segment', x=-97.1125, xend=-97.11920, y=31.5449, yend=31.5482,
    colour=I('red'), arrow = arrow(length=unit(0.4,"cm")), size = 1.5) +
  annotate('text', x=-97.1125, y=31.5445, label = 'Department of Statistical Science',
    colour = I('red'), size = 6)




baylorosm <- get_map(location = c(lon = -97.11922, lat = 31.54838),
  source = 'osm', zoom = 16)
ggmap(baylorosm)



data(zips)
ggmap(get_map(maptype = 'satellite', zoom = 8), fullpage = TRUE) +
  geom_polygon(aes(x = lon, y = lat, group = plotOrder),
    data = zips, colour = NA, fill = 'red', alpha = .2) +
  geom_path(aes(x = lon, y = lat, group = plotOrder),
    data = zips, colour = 'white', alpha = .4, size = .4)
# discrepancy likely due to different projections.  on to-do.

library(plyr)
zipsLabels <- ddply(zips, .(zip), function(df){
  df[1,c("area", "perimeter", "zip", "lonCent", "latCent")]
})
options('device')$device(width = 7.7, height = 6.7)
ggmap(get_map(maptype = 'satellite', zoom = 9), fullpage = TRUE) +
  geom_text(aes(x = lonCent, y = latCent, label = zip, size = area),
    data = zipsLabels, colour = I('red')) +
  scale_size(range = c(1.5,6))




# Crime data example

# format data
violent_crimes <- subset(crime,
  offense != 'auto theft' & offense != 'theft' & offense != 'burglary'
)

violent_crimes$offense <- factor(violent_crimes$offense,
  levels = c('robbery', 'aggravated assault', 'rape', 'murder')
)
levels(violent_crimes$offense) <- c('robbery', 'aggravated assault', 'rape', 'murder')


# get map and bounding box
houston <- get_map(location = 'houston', zoom = 14)
lat_range <- as.numeric(attr(houston, 'bb')[c('ll.lat','ur.lat')])
lon_range <- as.numeric(attr(houston, 'bb')[c('ll.lon','ur.lon')])
HoustonMap <- ggmap(houston)
theme_set(theme_bw())

# make bubble chart
options('device')$device(width = 9.25, height = 7.25)

HoustonMap +
   geom_point(aes(x = lon, y = lat, colour = offense, size = offense), data = violent_crimes) +
   scale_x_continuous('Longitude', limits = lon_range) +
   scale_y_continuous('Latitude', limits = lat_range) +
   opts(title = 'Violent Crime Bubble Map of Downtown Houston') +
   scale_colour_discrete('Offense', labels = c('Robery','Aggravated\\nAssault','Rape','Murder')) +
   scale_size_discrete('Offense', labels = c('Robery','Aggravated\\nAssault','Rape','Murder'))



# make contour plot
violent_crimes <- subset(violent_crimes,
  lat_range[1] <= lat & lat <= lat_range[2] &
  lon_range[1] <= lon & lon <= lon_range[2]
)

HoustonMap +
  stat_density2d(aes(x = lon, y = lat, colour = ..level..),
    bins = 4, fill = NA, alpha = .5, size = 2, data = violent_crimes) +
  scale_colour_gradient2('Violent\\nCrime\\nDensity',
    low = 'darkblue', mid = 'orange', high = 'red', midpoint = 35) +
  scale_x_continuous('Longitude', limits = lon_range) +
  scale_y_continuous('Latitude', limits = lat_range) +
  opts(title = 'Violent Crime Contour Map of Downtown Houston')
  # note current ggplot2 issue of the color of contours

# the fill aesthetic is now available -
HoustonMap +
  stat_density2d(aes(x = lon, y = lat, fill = ..level..),
    bins = 6, alpha = 1/5, geom = 'polygon', data = violent_crimes, ) +
  scale_fill_gradient('Violent\\nCrime\\nDensity') +
  scale_x_continuous('Longitude', limits = lon_range) +
  scale_y_continuous('Latitude', limits = lat_range) +
  opts(title = 'Violent Crime Contour Map of Downtown Houston') +
  guides(fill = guide_colorbar(override.aes = list(alpha = 1)))
  # note current issue of the cliped contours


options('device')$device(width = 9.25, height = 7.25)
HoustonMap +
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
    bins = 6, geom = 'polygon', data = violent_crimes) +
  scale_fill_gradient2('Violent\\nCrime\\nDensity',
    low = 'white', mid = 'orange', high = 'red', midpoint = 500) +
  scale_x_continuous('Longitude', limits = lon_range) +
  scale_y_continuous('Latitude', limits = lat_range) +
  scale_alpha(range = c(.1, .45), guide = FALSE) +
  opts(title = 'Violent Crime Contour Map of Downtown Houston') +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))

# we can also add insets
HoustonMap +
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
    bins = 6, geom = 'polygon', data = violent_crimes) +
  scale_fill_gradient2('Violent\\nCrime\\nDensity',
    low = 'white', mid = 'orange', high = 'red', midpoint = 500) +
  scale_x_continuous('Longitude', limits = lon_range) +
  scale_y_continuous('Latitude', limits = lat_range) +
  scale_alpha(range = c(.1, .45), guide = FALSE) +
  opts(title = 'Violent Crime Contour Map of Downtown Houston') +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) +
  ggmap:::annotation_custom(
    grob = ggplotGrob(ggplot() +
      stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
        bins = I(6), geom = 'polygon', data = violent_crimes) +
      scale_fill_gradient2('Violent\\nCrime\\nDensity',
        low = 'white', mid = 'orange', high = 'red', midpoint = 500, guide = FALSE) +
      scale_x_continuous('Longitude', limits = lon_range) +
      scale_y_continuous('Latitude', limits = lat_range) +
      scale_alpha(range = c(.1, .45), guide = FALSE) +
      theme_inset()
    ),
    xmin = attr(houston,'bb')$ll.lon +
      (7/10) * (attr(houston,'bb')$ur.lon - attr(houston,'bb')$ll.lon),
    xmax = Inf,
    ymin = -Inf,
    ymax = attr(houston,'bb')$ll.lat +
      (3/10) * (attr(houston,'bb')$ur.lat - attr(houston,'bb')$ll.lat)
  )



df <- data.frame(
  lon = rep(seq(-95.39, -95.35, length.out = 8), each = 20),
  lat = sapply(
    rep(seq(29.74, 29.78, length.out = 8), each = 20),
    function(x) rnorm(1, x, .002)
  ),
  class = rep(letters[1:8], each = 20)
)

qplot(lon, lat, data = df, geom = 'boxplot', fill = class)

HoustonMap +
  geom_boxplot(aes(x = lon, y = lat, fill = class), data = df)


# using the base_layer argument
df <- data.frame(
  x = rnorm(100, -95.36258, .05),
  y = rnorm(100,  29.76196, .05)
)
ggmap(get_map(), base_layer = ggplot(aes(x = x, y = y), data = df)) +
  geom_point(colour = 'red')

# using the maprange argument
ggmap(get_map(), maprange = TRUE,
  base_layer = ggplot(aes(x = x, y = y), data = df)) +
  geom_point(colour = 'red')



# faceting is available via the base_layer argument
df <- data.frame(
  x = rnorm(10*100, -95.36258, .075),
  y = rnorm(10*100,  29.76196, .075),
  year = rep(paste('year',format(1:10)), each = 100)
)
ggmap(get_map(), base_layer = ggplot(aes(x = x, y = y), data = df)) +
  geom_point() +  facet_wrap(~ year)


# a neat example
df <- data.frame(
  x = rnorm(10*100, -95.36258, .05),
  y = rnorm(10*100,  29.76196, .05),
  year = rep(paste('year',format(1:10)), each = 100)
)
for(k in 0:9){
  df$x[1:100 + 100*k] <- df$x[1:100 + 100*k] + sqrt(.05)*cos(2*pi*k/10)
  df$y[1:100 + 100*k] <- df$y[1:100 + 100*k] + sqrt(.05)*sin(2*pi*k/10)
}

options('device')$device(width = 10.93, height = 7.47)
ggmap(get_map(), maprange = TRUE, expand = TRUE,
  base_layer = ggplot(aes(x = x, y = y), data = df)) +
  stat_density2d(aes(fill = ..level.., alpha = ..level..),
    bins = 4, geom = 'polygon') +
  scale_fill_gradient2(low = 'white', mid = 'orange', high = 'red', midpoint = 10) +
  scale_alpha(range = c(.2, .75), guide = FALSE) +
  facet_wrap(~ year)

ggmap(get_map(), maprange = TRUE, expand = TRUE,
  base_layer = ggplot(aes(x = x, y = y), data = df)) +
  stat_density2d(aes(fill = ..level.., alpha = ..level..),
    bins = 20, geom = 'polygon') +
  scale_fill_gradient2(low = 'white', mid = 'orange', high = 'red', midpoint = 10) +
  scale_alpha(range = c(.2, .75), guide = FALSE) +
  facet_wrap(~ year)


# crime by month
levels(violent_crimes$month) <- paste(
  toupper(substr(levels(violent_crimes$month),1,1)),
  substr(levels(violent_crimes$month),2,20), sep = ''
)
houston <- get_map(location = 'houston', zoom = 14, source = 'osm', color = 'bw')
HoustonMap <- ggmap(houston,
  base_layer = ggplot(aes(x = lon, y = lat), data = violent_crimes)
  )
options('device')$device(width = 8.62, height = 7.48)
HoustonMap +
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
    bins = I(5), geom = 'polygon', data = violent_crimes) +
  scale_fill_gradient2('Violent\\nCrime\\nDensity',
    low = 'white', mid = 'orange', high = 'red', midpoint = 500) +
  scale_x_continuous('Longitude', limits = lon_range) +
  scale_y_continuous('Latitude', limits = lat_range) +
  facet_wrap(~ month) +
  scale_alpha(range = c(.2, .55), guide = FALSE) +
  opts(title = 'Violent Crime Contour Map of Downtown Houston by Month') +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))




# neat example with distances


origin <- 'marrs mclean science, baylor university'
gc_origin <- geocode(origin)
destinations <- data.frame(
  place = c("Administration", "Baseball Stadium", "Basketball Arena",
    "Salvation Army", "HEB Grocery", "Cafe Cappuccino", "Ninfa's Mexican",
    "Dr Pepper Museum", "Buzzard Billy's", "Mayborn Museum","Flea Market"
  ),
  address = c("pat neff hall, baylor university", "baylor ballpark",
    "ferrell center", "1225 interstate 35 s, waco, tx",
    "1102 speight avenue, waco, tx", "100 n 6th st # 100, waco, tx",
    "220 south 3rd street, waco, tx", "300 south 5th street, waco, tx",
    "100 north jack kultgen expressway, waco, tx",
    "1300 south university parks drive, waco, tx",
    "2112 state loop 491, waco, tx"
  ),
  stringsAsFactors = FALSE
)
gc_dests <- geocode(destinations$address)
(dist <- mapdist(origin, destinations$address, mode = 'bicycling'))

dist <- within(dist, {
  place = destinations$place
  fromlon = gc_origin$lon
  fromlat = gc_origin$lat
  tolon = gc_dests$lon
  tolat = gc_dests$lat
})
dist$minutes <- cut(dist$minutes, c(0,3,5,7,10,Inf),
  labels = c('0-3','3-5', '5-7', '7-10', '10+'))

library(scales)
qmap('baylor university', zoom = 14, maprange = TRUE, fullpage = TRUE,
    base_layer = ggplot(aes(x = lon, y = lat), data = gc_origin),
    legend = 'bottomright') +
  geom_rect(aes(
    x = tolon, y = tolat,
    xmin = tolon-.00028*nchar(place), xmax = tolon+.00028*nchar(place),
    ymin = tolat-.0005, ymax = tolat+.0005, fill = minutes, colour = 'black'
  ), alpha = .7, data = dist) +
  geom_text(aes(x = tolon, y = tolat, label = place, colour = 'white'), size = 3, data = dist) +
  geom_rect(aes(
    xmin = lon-.004, xmax = lon+.004,
    ymin = lat-.00075, ymax = lat+.00075, colour = 'black'
  ), alpha = .5, fill = I('green'), data = gc_origin) +
  geom_text(aes(x = lon, y = lat, label = 'My Office', colour = 'black'), size = 5) +
  scale_fill_manual('Minutes\\nAway\\nby Bike',
    values = colorRampPalette(c(muted('green'), 'blue', 'red'))(5)) +
  scale_colour_identity(guide = 'none') +
  opts(
    legend.direction = 'horizontal',
    legend.key.size = unit(2, 'lines')
  ) +
  guides(
    fill = guide_legend(
      title.theme = theme_text(size = 16, face = 'bold', colour = 'black'),
      label.theme = theme_text(size = 14, colour = 'black'),
      label.position = 'bottom',
      override.aes = list(alpha = 1)
    )
  )







states <- map_data('state')
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))

choro <- merge(states, arrests, sort = FALSE, by = 'region')
choro <- choro[order(choro$order), ]

legs_df <- route('mexico city', 'anchorage, alaska', alternatives = TRUE)
route_df <- legs2route(legs_df)

qmap('the united states', zoom = 3, maptype = 'satellite', fullpage = FALSE) +
  geom_polygon(aes(x = long, y = lat, colour = 'white',
      group = group, fill = assault),
    alpha = .4, size = .25, data = choro) +
  geom_path(aes(x = lon, y = lat, linetype = route), data = route_df, lineend = 'round') +
  scale_fill_gradient('Assaults', low = 'yellow', high = 'red') +
  scale_colour_identity(guide = 'none') +
  scale_x_continuous('Longitude', expand = c(0,0)) +
  scale_y_continuous('Latitude', expand = c(0,0)) +
  guides(fill = guide_colorbar()) +
  labs(linetype = 'Route')





}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \code{\link{get_map}}, \code{\link{qmap}}
}

