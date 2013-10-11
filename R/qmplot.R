#' Quick map plot
#'
#' \code{qmplot} is the ggmap equivalent to the ggplot2 function qplot and allows for 
#' the quick plotting of maps with data/models/etc.  
#' 
#' 
#' @param x longitude values
#' @param y latitude values
#' @param ... other aesthetics passed for each layer
#' @param data data frame to use (optional).  If not specified, will create 
#'   one, extracting vectors from the current environment.
#' @param zoom map zoom, see \code{\link{get_map}} 
#' @param source map source, see \code{\link{get_map}}
#' @param maptype map type, see \code{\link{get_map}}
#' @param extent how much of the plot should the map take up? "normal", "panel", or "device" (default)
#' @param legend "left", "right" (default), "bottom", "top", "bottomleft", "bottomright", "topleft", "topright", "none" (used with extent = "device") 
#' @param padding distance from legend to corner of the plot  (used with extent = "device") 
#' @param force force new map (don't use archived version)
#' @param darken vector of the form c(number, color), where number is in [0, 1] and color is a character string indicating the color of the darken.  0 indicates no darkening, 1 indicates a black-out.
#' @param mapcolor color ("color") or black-and-white ("bw")
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
#' \dontrun{
#'
#' qmplot(lon, lat, data = crime)
#' 
#' 
#' # only violent crimes
#' violent_crimes <- subset(crime,
#'   offense != "auto theft" & 
#'   offense != "theft" & 
#'   offense != "burglary"
#' )
#' 
#' # rank violent crimes
#' violent_crimes$offense <- 
#'   factor(violent_crimes$offense,
#'     levels = c("robbery", "aggravated assault", 
#'       "rape", "murder")
#'   )
#' 
#' # restrict to downtown
#' violent_crimes <- subset(violent_crimes,
#'   -95.39681 <= lon & lon <= -95.34188 &
#'    29.73631 <= lat & lat <=  29.78400
#' )
#'
#' theme_set(theme_bw())
#' 
#' qmplot(lon, lat, data = violent_crimes, colour = offense, darken = .5, 
#'   size = I(3.5), alpha = I(.6), legend = "topleft")
#' 
#' qmplot(lon, lat, data = violent_crimes, geom = c("point","density2d"))
#' qmplot(lon, lat, data = violent_crimes) + facet_wrap(~ offense)
#' qmplot(lon, lat, data = violent_crimes, extent = "panel") + facet_wrap(~ offense)
#' qmplot(lon, lat, data = violent_crimes, extent = "panel", colour = offense) + 
#'   facet_wrap(~ month)
#'
#' 
#' 
#' 
#' # doesn't quite work yet.... http://tile.stamen.com/terrain/#4/30.28/-87.21
#' qmplot(long, lat, xend = long + delta_long, 
#'   yend = lat + delta_lat, data = seals, geom = "segment")
#'
#' qmplot(long, lat, xend = long + delta_long, maptype = "toner", 
#'   color = I("red"), yend = lat + delta_lat, data = seals, 
#'   geom = "segment", zoom = 6)
#'
#' qmplot(long, lat, xend = long + delta_long, maptype = "watercolor", 
#'   yend = lat + delta_lat, data = seals, 
#'   geom = "segment", zoom = 6)
#' 
#'
#' library(scales)
#' library(grid)
#' options("device")$device(width = 4.98, height = 5.97)
#' qmplot(lon, lat, data = wind, size = I(.5), alpha = I(.5)) +
#'   ggtitle("NOAA Wind Report Sites")   
#'
#' # thin down data set...
#' s <- seq(1, 227, 8)
#' thinwind <- subset(wind, 
#'   lon %in% unique(wind$lon)[s] & 
#'   lat %in% unique(wind$lat)[s]
#' )
#' 
#' # for some reason adding arrows to the following plot bugs
#' theme_set(theme_bw(18))
#' options("device")$device(width = 6.13, height = 7.04)
#' qmplot(lon, lat, data = thinwind, geom = "tile", fill = spd, alpha = spd,
#'     legend = "bottomleft") +
#'   geom_leg(aes(xend = lon + delta_lon, yend = lat + delta_lat)) +
#'   scale_fill_gradient2("Wind Speed\nand\nDirection", 
#'     low = "green", mid = muted("green"), high = "red") +
#'   scale_alpha("Wind Speed\nand\nDirection", range = c(.1, .75)) +
#'   guides(fill = guide_legend(), alpha = guide_legend())
#' 
#' 
#' 
#' 
#' ## kriging
#' ############################################################
#' 
#' library(ggmap)
#' library(lattice)
#' library(sp)
#' library(rgdal)
#' 
#' # load in and format the meuse dataset (see bivand, pebesma, and gomez-rubio)
#' data(meuse)
#' coordinates(meuse) <- c("x", "y")
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#' meuse <- spTransform(meuse, CRS("+proj=longlat +datum=WGS84"))
#' 
#' # plot
#' plot(meuse)
#' 
#' df <- as.data.frame(slot(meuse, "coords"))
#' df <- data.frame(df, slot(meuse, "data"))
#' 
#' qmplot(x, y, size = cadmium, alpha = I(.75), color = I("green"), 
#'   data = df, zoom = 14, legend = "topleft",
#'   source = "google", maptype = "satellite", darken = .2
#' ) + scale_size("Cadmium\n(ppm)")
#' 
#' 
#' # load in the meuse.grid dataset (looking toward kriging)
#' library(gstat)
#' data(meuse.grid)
#' coordinates(meuse.grid) <- c("x", "y")
#' proj4string(meuse.grid) <- CRS("+init=epsg:28992")
#' meuse.grid <- spTransform(meuse.grid, CRS("+proj=longlat +datum=WGS84"))
#' mg <- as.data.frame(slot(meuse.grid, "coords"))
#' mg <- data.frame(mg, slot(meuse.grid, "data"))
#' 
#' # plot it
#' qmplot(x, y, data = mg, source = "google")
#' 
#' 
#' # using linear regression - surface trend analysis
#' tsa <- krige(log(zinc) ~ 1, meuse, meuse.grid)
#' mg$pred <- slot(tsa, "data")$var1.pred
#' qmplot(x, y, data = mg, source = "google", color = pred, 
#'     size = I(2.5), shape = I(15),
#'     alpha = I(3/5), darken = .3, legend = "topleft") +
#'   scale_color_gradient("Predicted\nLog Zinc",
#'     low = "green", high = "red"
#'   )
#' 
#' tsa <- krige(log(zinc) ~ 1, meuse, meuse.grid, degree = 2)
#' mg$pred <- slot(tsa, "data")$var1.pred
#' qmplot(x, y, data = mg, source = "google", color = pred, 
#'     size = I(2.5), shape = I(15),
#'     alpha = I(3/5), darken = .3, legend = "topleft") +
#'   scale_color_gradient("Predicted\nLog Zinc",
#'     low = "green", high = "red"
#'   )
#'   
#'   
#' # ordinary kriging
#' vgram <- variogram(log(zinc) ~ sqrt(dist), meuse)
#' vgramFit <- fit.variogram(vgram, vgm(1, "Exp", 300, 1))
#' ordinaryKrige <- krige(log(zinc) ~ 1, meuse, meuse.grid, vgramFit)
#' mg$pred <- slot(ordinaryKrige, "data")$var1.pred
#' qmplot(x, y, data = mg, source = "google", color = pred, 
#'     size = I(2.5), shape = I(15),
#'     alpha = I(3/5), darken = .3, legend = "topleft") +
#'   scale_color_gradient("Predicted\nLog Zinc",
#'     low = "green", high = "red"
#'   )
#'   
#' # universal kriging
#' universalKrige <- krige(log(zinc) ~ sqrt(dist), meuse, meuse.grid, vgramFit)
#' mg$pred <- slot(universalKrige, "data")$var1.pred
#' qmplot(x, y, data = mg, source = "google", color = pred,
#'     size = I(2.5), shape = I(15),
#'     alpha = I(3/4), darken = .3, legend = "topleft") +
#'   scale_color_gradient("Predicted\nLog Zinc",
#'     low = "green", high = "red"
#'   )  
#'   
#'   
#'   
#' # kriging with data  
#' qmplot(x, y, data = mg, source = "google", color = pred,
#'     size = I(2.5), shape = I(15),
#'     alpha = I(3/4), darken = .3, legend = "topleft") +
#'   scale_color_gradient("Predicted\nLog Zinc",
#'     low = "green", high = "red"
#'   ) +
#'   geom_point(
#'     aes(x = x, y = y, size = log(zinc)), data = df,
#'     color = "black", alpha = 1/2
#'   ) +
#'   scale_size("Observed\nLog Zinc")
#' 
#'
#' 
#' 
#' 
#' 
#' 
#' }
#'
qmplot <- function(x, y, ..., data, zoom, source = "stamen", maptype = "terrain",
  extent = "device", legend = "right", padding = .02, force = FALSE,
  darken = c(0, "black"), mapcolor = "color",
  facets = NULL, margins = FALSE, geom = "auto", stat = list(NULL), 
  position = list(NULL), xlim = c(NA, NA), ylim = c(NA, NA), main = NULL, f = 0.05, 
  xlab = deparse(substitute(x)), ylab = deparse(substitute(y)))
{
	
  argnames <- names(as.list(match.call(expand.dots=FALSE)[-1]))
  arguments <- as.list(match.call()[-1])
  
  args <- as.list(match.call(expand.dots = TRUE)[-1])  
  argsgiven <- names(args)
  if("mapcolour" %in% argsgiven) mapcolor <- eval(args$mapcolour)
  
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
      stop("y must be provided for quickmap.", call. = FALSE)
    } else {
      if (missing(x)) {
        aesthetics$x <- bquote(seq_along(.(y)), aesthetics)
      }
      geom[geom == "auto"] <- "point"
    }
  }

  env <- parent.frame()
  
  # calculate map dimensions    
  bbox <- make_bbox(
    lon = data[,deparse(substitute(x))], 
    lat = data[,deparse(substitute(y))],
    f = f
  )    
  lon_range <- bbox[c("left","right")]
  lat_range <- bbox[c("bottom","top")]  
  
  
  # compute zoom
  if(missing(zoom)){
    zoom <- calc_zoom(lon_range, lat_range)
    message(paste0("Using zoom = ", zoom, "..."))
  }
  
  
  # get map
  if(source == "google"){
    map <- get_map(location = c(mean(lon_range), mean(lat_range)), zoom = zoom, 
      source = source, color = mapcolor,
      maptype = maptype, force = force
    )  	
  } else { # bounding box
    map <- get_map(location = bbox, zoom = zoom, 
      source = source, color = mapcolor,
      maptype = maptype, force = force
    )
  }
  xmin <- attr(map, "bb")$ll.lon
  xmax <- attr(map, "bb")$ur.lon
  ymin <- attr(map, "bb")$ll.lat
  ymax <- attr(map, "bb")$ur.lat  
  
  
  # check darken
  stopifnot(0 <= as.numeric(darken[1]) && as.numeric(darken[1]) <= 1)
  if(length(darken) == 1 & is.numeric(darken)) darken <- c(darken, "black")
  
  
  # initialize plot
  p <- ggplot(data, aesthetics, environment = env) +
    inset_raster(map, xmin, xmax, ymin, ymax) + 
    annotate("rect", xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, 
  	  fill = darken[2], alpha = as.numeric(darken[1])) +
    coord_map(projection = "mercator")
    
    
  # enforce extent
  if(extent == "normal"){
    # nothing
  } else if(extent == "panel"){
  	p <- p +
      scale_x_continuous(lim = c(xmin, xmax), expand = c(0,0)) +
      scale_y_continuous(lim = c(ymin, ymax), expand = c(0,0))           
  } else if(extent == "device"){
  	p <- p +
      scale_x_continuous(lim = c(xmin, xmax), expand = c(0,0)) +
      scale_y_continuous(lim = c(ymin, ymax), expand = c(0,0)) +
      theme_nothing(legend = TRUE)    
      
      
    # legend for full device map
    if(legend %in% c("topleft","topright","bottomleft","bottomright")){
      if(legend == "bottomleft"){
        lp <- c(padding, padding)
        lj <- c(0,0)
      } else if(legend == "topleft"){
        lp <- c(padding, 1-padding)
        lj <- c(0,1)
      } else if(legend == "bottomright"){
        lp <- c(1-padding, padding)
        lj <- c(1,0)
      } else if(legend == "topright"){
        lp <- c(1-padding, 1-padding)
        lj <- c(1,1)
      }
      p <- p + theme(
        legend.position = lp, legend.justification = lj,
        legend.background = element_rect(
          fill = "white", colour = "gray80", size = .2
        )        
      )
    } else if(legend %in% c("left","right","bottom","top")){
      p <- p + theme(legend.position = legend)
    } # else legend = "none" as part of theme_nothing()
  }
      
    
  
  if (is.null(facets)) {
    p <- p + facet_null()
  } else if (is.formula(facets) && length(facets) == 2) {
    p <- p + facet_wrap(facets)
  } else {
    p <- p + facet_grid(facets = deparse(facets), margins = margins)
  }
  
  if (!is.null(main)) p <- p + theme("title" = main)

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
  

  
  p
}