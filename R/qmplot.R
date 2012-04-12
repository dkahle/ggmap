#' Quick map plot
#'
#' \code{qmplot} is the ggmap equivalent to the ggplot2 function qplot and allows for 
#' the quick plotting of maps with data/models/etc.  qmplot is still experimental.
#' 
#' 
#' @param x longitude values
#' @param y latitude values
#' @param ... other aesthetics passed for each layer
#' @param data data frame to use (optional).  If not specified, will create 
#'   one, extracting vectors from the current environment.
#' @param zoom map zoom, see \code{\link{get_map}}
#' @param source map source, see \code{\link{get_map}}
#' @param fullpage should the map fill the device
#' @param facets faceting formula to use.  Picks \code{\link{facet_wrap}} or
#'   \code{\link{facet_grid}} depending on whether the formula is one sided
#'   or two-sided
#' @param margins whether or not margins will be displayed
#' @param geom character vector specifying geom to use.  defaults to 
#'  "point"
#' @param stat character vector specifying statistics to use
#' @param position character vector giving position adjustment to use
#' @param xlim limits for x axis
#' @param ylim limits for y axis
#' @param main character vector or expression for plot title
#' @param f number specifying the fraction by which the range should be extended
#' @param xlab character vector or expression for x axis label
#' @param ylab character vector or expression for y axis label
#' @export
#' @examples
#' \donttest{
#' qmplot(lon, lat, data = crime)
#' qmplot(lon, lat, data = crime, colour = offense)
#' qmplot(lon, lat, data = crime, geom = c('point','density2d'))
#' qmplot(lon, lat, data = crime) + facet_wrap(~ offense)
#' qmplot(lon, lat, data = crime, fullpage = FALSE) + facet_wrap(~ offense)
#' qmplot(lon, lat, data = crime, fullpage = FALSE, colour = offense) + 
#'   facet_wrap(~ month)
#' qmplot(long, lat, xend = long + delta_long, 
#'   yend = lat + delta_lat, data = seals, geom = 'segment')
#' 
#'
#' library(scales)
#' library(grid)
#' qmplot(lon, lat, data = wind, size = I(.5), alpha = I(.5))
#' 
#' # thin down data set...
#' s <- seq(1, 227, 8)
#' thinwind <- subset(wind, 
#'   lon %in% unique(wind$lon)[s] & 
#'   lat %in% unique(wind$lat)[s]
#' )
#' 
#' # for some reason adding arrows to the following plot bugs
#' qmplot(lon, lat, data = thinwind, geom = 'tile', fill = spd, alpha = spd) +
#'   geom_leg(aes(xend = lon + delta_lon, yend = lat + delta_lat)) +
#'   scale_fill_gradient2(low = 'green', mid = muted('green'), high = 'red') +
#'   scale_alpha(range = c(.1, .75))
#' 
#' 
#' 
#' 
#' }
#'
qmplot <- function(x, y, ..., data, zoom, source = 'stamen', fullpage = TRUE, 
  facets = NULL, margins = FALSE, geom = "auto", stat = list(NULL), position = list(NULL), 
  xlim = c(NA, NA), ylim = c(NA, NA), main = NULL, f = 0.05,
  xlab = deparse(substitute(x)), ylab = deparse(substitute(y)))
{
	
  argnames <- names(as.list(match.call(expand.dots=FALSE)[-1]))
  arguments <- as.list(match.call()[-1])
  
  .all_aesthetics <- unlist(getAnywhere(.all_aesthetics)[1:42])  
  aesthetics <- compact(arguments[.all_aesthetics])
  aesthetics <- aesthetics[!ggplot2:::is.constant(aesthetics)]
  aes_names <- names(aesthetics)
  aesthetics <- ggplot2:::rename_aes(aesthetics)
  class(aesthetics) <- "uneval"
  
  if (missing(data)) {
    # If data not explicitly specified, will be pulled from workspace
    data <- data.frame()

    # Faceting variables must be in a data frame, so pull those out
    facetvars <- all.vars(facets)
    facetvars <- facetvars[facetvars != "."]
    names(facetvars) <- facetvars
    facetsdf <- as.data.frame(lapply(facetvars, get))
    if (nrow(facetsdf)) data <- facetsdf
  }

  # Work out plot data, and modify aesthetics, if necessary
  if ("auto" %in% geom) {
    if (stat == "qq" || "sample" %in% aes_names) {
      geom[geom == "auto"] <- "point"
      stat <- "qq"
    } else if (missing(y)) {
      stop('y must be provided for quickmap.', call. = FALSE)
    } else {
      if (missing(x)) {
        aesthetics$x <- bquote(seq_along(.(y)), aesthetics)
      }
      geom[geom == "auto"] <- "point"
    }
  }

  env <- parent.frame()
  
  # calculate map dimensions



  lons <- data[,deparse(substitute(x))]  
  lon_range <- extendrange(range(lons, na.rm = TRUE))
  lats <- data[,deparse(substitute(y))]
  lat_range <- extendrange(range(lats, na.rm = TRUE))  
  bbox <- c(left = lon_range[1], bottom = lat_range[1],
    right = lon_range[2], top = lat_range[2])
    
  bbox <- make_bbox(
    lon = data[,deparse(substitute(x))], 
    lat = data[,deparse(substitute(y))],
    f = f
  )    
  
  # compute zoom
  if(missing(zoom)){
    lonlength <- diff(lon_range)
    latlength <- diff(lat_range)    
    zoomlon <- ceiling( log2( 360*2 / lonlength) )
    zoomlat <- ceiling( log2( 180*2 / latlength) )    
    zoom <- max(zoomlon, zoomlat)
  }
  
  # get map
  map <- get_map(location = bbox, zoom = zoom, source = source)
  xmin <- attr(map, "bb")$ll.lon
  xmax <- attr(map, "bb")$ur.lon
  ymin <- attr(map, "bb")$ll.lat
  ymax <- attr(map, "bb")$ur.lat  
  
  # initialize plot
  p <- ggplot(data, aesthetics, environment = env) +
    ggmap:::annotation_raster(map, xmin, xmax, ymin, ymax) +
    coord_map(projection = "mercator")
    
  
  if (is.null(facets)) {
    p <- p + facet_null()
  } else if (is.formula(facets) && length(facets) == 2) {
    p <- p + facet_wrap(facets)
  } else {
    p <- p + facet_grid(facets = deparse(facets), margins = margins)
  }
  
  if (!is.null(main)) p <- p + opts("title" = main)

  # Add geoms/statistics
  if (is.proto(position)) position <- list(position)
  
  mapply(function(g, s, ps) {
    if(is.character(g)) g <- ggplot2:::Geom$find(g)
    if(is.character(s)) s <- ggplot2:::Stat$find(s)
    if(is.character(ps)) ps <- ggplot2:::Position$find(ps)

    params <- arguments[setdiff(names(arguments), c(aes_names, argnames))]
    params <- lapply(params, eval, parent.frame(n=1))
    
    p <<- p + layer(geom=g, stat=s, geom_params=params, stat_params=params, position=ps)
  }, geom, stat, position)
  
  if (!missing(xlab)) p <- p + xlab(xlab)
  if (!missing(ylab)) p <- p + ylab(ylab)
  
  if (!missing(xlim)) p <- p + xlim(xlim)
  if (!missing(ylim)) p <- p + ylim(ylim)
  
  if(fullpage) p <- p +
    scale_x_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) + 
    theme_nothing()
  
  p
}