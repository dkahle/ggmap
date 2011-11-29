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
#' (HoustonMap <- ggmapplot(ggmap()))
#' 
#' \dontrun{
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
#'   scale_size(to = c(.1, .75), legend = FALSE)
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
#' if(substr(.Platform$pkgType,1,3) == 'mac'){device <- 'quartz'} else {device <- 'x11'}
#' eval(call(device, width = 6, height = 5.2))
#' ggmapplot(ggmap(maptype = 'satellite', zoom = 9), fullpage = TRUE) +
#'   geom_text(aes(x = lonCent, y = latCent, label = zip, size = area), 
#'     data = zipsLabels, colour = I('red')) +
#'   scale_size(to = c(1.5,6))
#' 
#' 
#' 
#' 
#' # Crime data example
#' 
#' # get map
#' houston <- ggmap(location = 'houston', zoom = 11)
#' lat_range <- range(houston$lat)
#' lon_range <- range(houston$lon)
#' 
#' # restrict to violent crimes
#' violent_crimes <- subset(crime,
#'   offense != 'Auto Theft' & offense != 'Theft' & offense != 'Burglary'
#' )
#' 
#' # contour plot
#' if(length(grep('apple', R.version$platform)) > 0){device <- 'quartz'} else {device <- 'x11'}
#' eval(call(device, width = 9.25, height = 7.5))
#' 
#' ggmapplot(houston) + theme_bw() +
#'   stat_density2d(aes(x = lon, y = lat, colour = ..level..), 
#'     bins = I(6), fill = NA, alpha = I(1/2), size = I(.75), data = violent_crimes) +
#'   scale_colour_gradient2('Violent\nCrime\nDensity', 
#'     low = 'darkblue', mid = 'orange', high = 'red', midpoint = 35) + 
#'   scale_x_continuous('Longitude', limits = lon_range) + 
#'   scale_y_continuous('Latitude', limits = lat_range) +
#'   opts(title = 'Violent Crime Contour Map of Houston')
#'   
#' ggmapplot(houston, fullpage = TRUE) +
#'   stat_density2d(aes(x = lon, y = lat, colour = ..level..), 
#'     bins = I(6), fill = NA, alpha = I(1/2), size = I(1.5), data = violent_crimes) +
#'   scale_colour_gradient2(legend = FALSE, 
#'     low = 'darkblue', mid = 'orange', high = 'red', midpoint = 35) + 
#'   xlim(lon_range) + ylim(lat_range)
#' 
#' } 
ggmapplot <- function(ggmap, fullpage = FALSE, regularize = TRUE, ...){
  require(ggplot2)
  
  # dummies to trick R CMD check   
  lon <- NULL; rm(lon); lat <- NULL; rm(lat); fill <- NULL; rm(fill);   
  
  if(class(ggmap)[1] != 'ggmap'){
    stop('ggmapplot plots objects of class ggmap, see ?ggmap', call. = FALSE)	
  }

  # make base plot
  p <- ggplot() + geom_tile(aes(x = lon, y = lat, fill = fill), data = ggmap)

  # set scales
  p <- p + scale_fill_identity(legend = FALSE) + coord_equal() 
    
  # fullpage?
  if(fullpage) p <- p + theme_nothing()
  
  p
}