#' Plot a ggmap.
#'
#' ggmapplot plots the data.frame from \code{\link{ggmap}}.
#' 
#' @param ggmap an object of class ggmap (from function ggmap)
#' @param fullpage logical; should the map take up the entire viewport?
#' @param regularize logical; should the longitude and latitude coordinates be regularized?
#' @param ... ...
#' @return a ggplot object
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{ggmap}} 
#' @export
#' @examples
#' 
#' \dontrun{ 
#' hdf <- ggmap()
#' (HoustonMap <- ggmapplot(hdf)
#' 
#' require(MASS)
#' mu <- c(-95.3632715, 29.7632836); nDataSets <- sample(4:10,1)
#' chkpts <- NULL
#' for(k in 1:nDataSets){
#'   a <- rnorm(2); b <- rnorm(2); si <- 1/3000 * (outer(a,a) + outer(b,b))
#'   chkpts <- rbind(chkpts, cbind(mvrnorm(rpois(1,50), jitter(mu, .01), si), k))	
#' }
#' chkpts <- data.frame(chkpts)
#' names(chkpts) <- c('lon', 'lat','class')
#' chkpts$class <- factor(chkpts$class)
#' qplot(lon, lat, data = chkpts, colour = class)
#'
#' HoustonMap + 
#'   geom_point(aes(x = lon, y = lat, colour = class), data = chkpts, alpha = .5)
#'
#' HoustonMap + 
#'   stat_density2d(aes(x = lon, y = lat, size = ..density.., colour = class), 
#'     geom = 'point', alpha = I(1/2), data = chkpts, contour = FALSE) + 
#'   scale_size(range = c(.1, .75), guide = 'none')  
#'  
#' 
#' HoustonMap <- ggmapplot(ggmap(maptype = 'satellite'), fullpage = TRUE) 
#' HoustonMap +
#'   stat_density2d(aes(x = lon, y = lat, colour = class), data = chkpts, bins = 5)
#'
#' ggmapplot(ggmap('Paris', verbose = TRUE), fullpage = TRUE)
#'
#'
#'
#' baylor <- ggmap('baylor university', zoom = 15,
#'   maptype = 'satellite', regularize = FALSE, verbose = TRUE)
#'
#' ggmapplot(baylor) + theme_bw() +
#'   annotate('rect', xmin=-97.11920, ymin=31.5439, xmax=-97.101, ymax=31.5452, 
#'     fill = I('black'), alpha = I(3/4)) + 
#'   annotate('segment', x=-97.110, xend=-97.11920, y=31.5450, yend=31.5482, 
#'     colour=I('red'), arrow = arrow(length=unit(0.3,"cm")), size = 1.5) +
#'   annotate('text', x=-97.110, y=31.5445, label = 'Department of Statistical Science', 
#'     colour = I('red'), size = 6) + 
#'   labs(x = 'Longitude', y = 'Latitude') + opts(title = 'Baylor University')
#'
#'
#'
#'
#' baylor <- ggmap('baylor university', zoom = 16,
#'   maptype = 'satellite', regularize = FALSE, verbose = TRUE)
#'
#' ggmapplot(baylor, fullpage = TRUE) +  
#'   annotate('rect', xmin=-97.1164, ymin=31.5441, xmax=-97.1087, ymax=31.5449,   
#'     fill = I('black'), alpha = I(3/4)) + 
#'   annotate('segment', x=-97.1125, xend=-97.11920, y=31.5449, yend=31.5482, 
#'     colour=I('red'), arrow = arrow(length=unit(0.4,"cm")), size = 1.5) +
#'   annotate('text', x=-97.1125, y=31.5445, label = 'Department of Statistical Science', 
#'     colour = I('red'), size = 6)
#'
#'
#'
#'  
#' baylor <- ggmap(center = c(lat = 31.54838, lon = -97.11922), zoom = 19,
#'   maptype = 'satellite', regularize = FALSE, verbose = TRUE)
#' ggmapplot(baylor, fullpage = TRUE)
#' 
#' 
#' 
#' # some playing around with the scale parameter is typically needed here.  see ?GetMap.OSM
#' baylorosm <- ggmap(center = c(lat = 31.54838, lon = -97.11922), source = 'osm', 
#'   verbose = TRUE, zoom = 16, scale = 10000)
#' ggmapplot(baylorosm, fullpage = TRUE)
#' 
#' 
#' 
#' data(zips)
#' ggmapplot(ggmap(maptype = 'satellite', zoom = 9), fullpage = TRUE) +
#'   geom_path(aes(x = lon, y = lat, group = plotOrder), data = zips, colour = I('red'), size = I(.4))
#' # adjust device size to get rid of horizontal lines  
#' 
#' library(plyr)
#' zipsLabels <- ddply(zips, .(zip), function(df){
#'   df[1,c("area", "perimeter", "zip", "lonCent", "latCent")]
#' })
#' options('device')$device(width = 7.7, height = 6.7)
#' ggmapplot(ggmap(maptype = 'satellite', zoom = 9), fullpage = TRUE) +
#'   geom_text(aes(x = lonCent, y = latCent, label = zip, size = area), 
#'     data = zipsLabels, colour = I('red')) +
#'   scale_size(range = c(1.5,6))
#' 
#' 
#' 
#' 
#' # Crime data example
#' 
#' # format data
#' violent_crimes <- subset(crime,
#'   offense != 'auto theft' & offense != 'theft' & offense != 'burglary'
#' )
#' 
#' violent_crimes$offense <- factor(violent_crimes$offense, 
#'   levels = c('robbery', 'aggravated assault', 'rape', 'murder')
#' )
#' levels(violent_crimes$offense) <- c('robbery', 'aggravated assault', 'rape', 'murder')
#' 
#' 
#' # get map and bounding box
#' houston <- ggmap(location = 'houston', zoom = 14)
#' lat_range <- as.numeric(attr(houston, 'bb')[c('ll.lat','ur.lat')])
#' lon_range <- as.numeric(attr(houston, 'bb')[c('ll.lon','ur.lon')])
#' HoustonMap <- ggmapplot(houston) 
#' theme_set(theme_bw())
#' 
#' # make bubble chart
#' options('device')$device(width = 9.25, height = 7.25)
#' 
#' HoustonMap +
#'    geom_point(aes(x = lon, y = lat, colour = offense, size = offense), data = violent_crimes) +
#'    scale_x_continuous('Longitude', limits = lon_range) + 
#'    scale_y_continuous('Latitude', limits = lat_range) +
#'    opts(title = 'Violent Crime Bubble Map of Downtown Houston') +
#'    scale_colour_discrete('Offense', labels = c('Robery','Aggravated\nAssault','Rape','Murder')) +
#'    scale_size_discrete('Offense', labels = c('Robery','Aggravated\nAssault','Rape','Murder'))   
#'    
#' 
#' 
#' # make contour plot
#' violent_crimes <- subset(violent_crimes,
#'   lat_range[1] <= lat & lat <= lat_range[2] &
#'   lon_range[1] <= lon & lon <= lon_range[2]
#' )
#' 
#' HoustonMap +
#'   stat_density2d(aes(x = lon, y = lat, colour = ..level..),
#'     bins = I(20), fill = NA, alpha = I(1/2), size = I(.75), data = violent_crimes) +
#'   scale_colour_gradient2('Violent\nCrime\nDensity',
#'     low = 'darkblue', mid = 'orange', high = 'red', midpoint = 35) +
#'   scale_x_continuous('Longitude', limits = lon_range) +
#'   scale_y_continuous('Latitude', limits = lat_range) +
#'   opts(title = 'Violent Crime Contour Map of Downtown Houston')
#' 
#' # the fill aesthetic is now available -
#' HoustonMap +
#'   stat_density2d(aes(x = lon, y = lat, fill = ..level..),
#'     bins = I(6), alpha = I(1/5), geom = 'polygon', data = violent_crimes, ) +
#'   scale_fill_gradient('Violent\nCrime\nDensity') +
#'   scale_x_continuous('Longitude', limits = lon_range) +
#'   scale_y_continuous('Latitude', limits = lat_range) +
#'   opts(title = 'Violent Crime Contour Map of Downtown Houston') +
#'   guides(fill = guide_colorbar(override.aes = list(alpha = 1)))
#'     
#'     
#' options('device')$device(width = 9.25, height = 7.25)
#' HoustonMap +
#'   stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
#'     bins = I(6), geom = 'polygon', data = violent_crimes) +
#'   scale_fill_gradient2('Violent\nCrime\nDensity',
#'     low = 'white', mid = 'orange', high = 'red', midpoint = 500) +
#'   scale_x_continuous('Longitude', limits = lon_range) +
#'   scale_y_continuous('Latitude', limits = lat_range) +
#'   scale_alpha(range = c(.1, .45), guide = FALSE) +
#'   opts(title = 'Violent Crime Contour Map of Downtown Houston') +
#'   guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10))    
#' 
#' # we can also add insets
#' HoustonMap +
#'   stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
#'     bins = I(6), geom = 'polygon', data = violent_crimes) +
#'   scale_fill_gradient2('Violent\nCrime\nDensity',
#'     low = 'white', mid = 'orange', high = 'red', midpoint = 500) +
#'   scale_x_continuous('Longitude', limits = lon_range) +
#'   scale_y_continuous('Latitude', limits = lat_range) +
#'   scale_alpha(range = c(.1, .45), guide = FALSE) +
#'   opts(title = 'Violent Crime Contour Map of Downtown Houston') +
#'   guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) +
#'   annotation_custom(
#'     grob = ggplotGrob(ggplot() +
#'       stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
#'         bins = I(6), geom = 'polygon', data = violent_crimes) +
#'       scale_fill_gradient2('Violent\nCrime\nDensity',
#'         low = 'white', mid = 'orange', high = 'red', midpoint = 500, guide = FALSE) +
#'       scale_x_continuous('Longitude', limits = lon_range) +
#'       scale_y_continuous('Latitude', limits = lat_range) +
#'       scale_alpha(range = c(.1, .45), guide = FALSE) +
#'       theme_inset()
#'     ), 
#'     xmin = attr(houston,'bb')$ll.lon + 
#'       (7/10) * (attr(houston,'bb')$ur.lon - attr(houston,'bb')$ll.lon),
#'     xmax = Inf,
#'     ymin = -Inf,
#'     ymax = attr(houston,'bb')$ll.lat + 
#'       (3/10) * (attr(houston,'bb')$ur.lat - attr(houston,'bb')$ll.lat)   
#'   )  
#' 
#' 
#' 
#' df <- data.frame(
#'   lon = rep(seq(-95.39, -95.35, length.out = 8), each = 20),
#'   lat = sapply(
#'     rep(seq(29.74, 29.78, length.out = 8), each = 20), 
#'     function(x) rnorm(1, x, .002)
#'   ),
#'   class = rep(letters[1:8], each = 20)
#' )  
#' 
#' qplot(lon, lat, data = df, geom = 'boxplot', fill = class)
#' 
#' HoustonMap +
#'   geom_boxplot(aes(x = lon, y = lat, fill = class), data = df)
#' 
#' 
#' 
#' 
#' 
#' } 
ggmapplot <- function(ggmap, fullpage = FALSE, regularize = TRUE, ...){
  
  # dummies to trick R CMD check   
  lon <- NULL; rm(lon); lat <- NULL; rm(lat); fill <- NULL; rm(fill);   
  ll.lon <- NULL; rm(ll.lon); ur.lon <- NULL; rm(ur.lon); 
  ll.lat <- NULL; rm(ll.lat); ur.lat <- NULL; rm(ur.lat);      
  
  if(class(ggmap)[1] != 'ggmap'){
    stop('ggmapplot plots objects of class ggmap, see ?ggmap', call. = FALSE)	
  }

  # make raster plot or tile plot
  if (inherits(ggmap, "raster")) { # raster
  	
    fourCorners <- expand.grid(
  	  lon = as.numeric(attr(ggmap, "bb")[,c('ll.lon','ur.lon')]),
  	  lat = as.numeric(attr(ggmap, "bb")[,c('ll.lat','ur.lat')])  	  
  	)  	
  	p <- ggplot(aes(x = lon, y = lat), data = fourCorners) + 
  	  geom_blank() +
  	  annotation_raster(ggmap, 
  	    xmin = attr(ggmap, "bb")$ll.lon, 
  	    xmax = attr(ggmap, "bb")$ur.lon, 
  	    ymin = attr(ggmap, "bb")$ll.lat, 
  	    ymax = attr(ggmap, "bb")$ur.lat
  	  )
  	  
  } else { # tile
    p <- ggplot() + geom_tile(aes(x = lon, y = lat, fill = fill), data = ggmap) +
      scale_fill_identity(guide = 'none')
  }

  # set scales
  p <- p + coord_equal() 
    
  # fullpage?
  if(fullpage) p <- p + theme_nothing()
  
  p
}