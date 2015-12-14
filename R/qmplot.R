#' Quick map plot
#'
#' \code{qmplot} is the ggmap equivalent to the ggplot2 function qplot and
#' allows for the quick plotting of maps with data/models/etc.
#'
#'
#' @param x longitude values
#' @param y latitude values
#' @param ... other aesthetics passed for each layer
#' @param data data frame to use (optional).  If not specified, will create one,
#'   extracting vectors from the current environment.
#' @param zoom map zoom, see \code{\link{get_map}}
#' @param source map source, see \code{\link{get_map}}
#' @param maptype map type, see \code{\link{get_map}}
#' @param extent how much of the plot should the map take up? "normal", "panel",
#'   or "device" (default)
#' @param legend "left", "right" (default), "bottom", "top", "bottomleft",
#'   "bottomright", "topleft", "topright", "none" (used with extent = "device")
#' @param padding distance from legend to corner of the plot  (used with extent
#'   = "device")
#' @param force force new map (don't use archived version)
#' @param darken vector of the form c(number, color), where number is in [0, 1]
#'   and color is a character string indicating the color of the darken.  0
#'   indicates no darkening, 1 indicates a black-out.
#' @param mapcolor color ("color") or black-and-white ("bw")
#' @param facets faceting formula to use.  Picks \code{\link{facet_wrap}} or
#'   \code{\link{facet_grid}} depending on whether the formula is one sided or
#'   two-sided
#' @param margins whether or not margins will be displayed
#' @param geom character vector specifying geom to use.  defaults to "point"
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
#'
#' \dontrun{ # these are skipped to conserve R check time
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
#' violent_crimes$offense <- factor(
#'   violent_crimes$offense,
#'   levels = c("robbery", "aggravated assault", "rape", "murder")
#' )
#'
#' # restrict to downtown
#' violent_crimes <- subset(violent_crimes,
#'   -95.39681 <= lon & lon <= -95.34188 &
#'    29.73631 <= lat & lat <=  29.78400
#' )
#'
#' theme_set(theme_bw())
#'
#' qmplot(lon, lat, data = violent_crimes, colour = offense,
#'   size = I(3.5), alpha = I(.6), legend = "topleft")
#'
#' qmplot(lon, lat, data = violent_crimes, geom = c("point","density2d"))
#' qmplot(lon, lat, data = violent_crimes) + facet_wrap(~ offense)
#' qmplot(lon, lat, data = violent_crimes, extent = "panel") + facet_wrap(~ offense)
#' qmplot(lon, lat, data = violent_crimes, extent = "panel", colour = offense, darken = .4) +
#'   facet_wrap(~ month)
#'
#'
#'
#'
#' qmplot(long, lat, xend = long + delta_long,
#'   color = I("red"), yend = lat + delta_lat, data = seals,
#'   geom = "segment", zoom = 5)
#'
#' qmplot(long, lat, xend = long + delta_long, maptype = "watercolor",
#'   yend = lat + delta_lat, data = seals,
#'   geom = "segment", zoom = 6)
#'
#'
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
#'
#' qmplot(lon, lat, data = thinwind, geom = "tile", fill = spd, alpha = spd,
#'     legend = "bottomleft") +
#'   geom_leg(aes(xend = lon + delta_lon, yend = lat + delta_lat)) +
#'   scale_fill_gradient2("Wind Speed\nand\nDirection",
#'     low = "green", mid = scales::muted("green"), high = "red") +
#'   scale_alpha("Wind Speed\nand\nDirection", range = c(.1, .75)) +
#'   guides(fill = guide_legend(), alpha = guide_legend())
#'
#'
#'
#'
#' ## kriging
#' ############################################################
#' # the below examples show kriging based on undeclared packages
#' # to better comply with CRAN's standards, we remove it from
#' # executing, but leave the code as a kind of case-study
#' # they also require the rgdal library
#'
#'
#' library(lattice)
#' library(sp)
#' library(rgdal)
#'
#' # load in and format the meuse dataset (see bivand, pebesma, and gomez-rubio)
#' data(meuse)
#' coordinates(meuse) <- c("x", "y")
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#' meuse <- spTransform(meuse, CRS("+proj=longlat +datum=WGS84"))
#'
#' # plot
#' plot(meuse)
#'
#' m <- data.frame(slot(meuse, "coords"), slot(meuse, "data"))
#' names(m)[1:2] <- c("lon", "lat")
#'
#' qmplot(lon, lat, data = m)
#' qmplot(lon, lat, data = m, zoom = 14)
#'
#'
#' qmplot(lon, lat, data = m, size = zinc,
#'   zoom = 14, source = "google", maptype = "satellite",
#'   alpha = I(.75), color = I("green"),
#'   legend = "topleft", darken = .2
#' ) + scale_size("Zinc (ppm)")
#'
#'
#'
#'
#'
#'
#'
#'
#' # load in the meuse.grid dataset (looking toward kriging)
#' library(gstat)
#' data(meuse.grid)
#' coordinates(meuse.grid) <- c("x", "y")
#' proj4string(meuse.grid) <- CRS("+init=epsg:28992")
#' meuse.grid <- spTransform(meuse.grid, CRS("+proj=longlat +datum=WGS84"))
#'
#' # plot it
#' plot(meuse.grid)
#'
#' mg <- data.frame(slot(meuse.grid, "coords"), slot(meuse.grid, "data"))
#' names(mg)[1:2] <- c("lon", "lat")
#'
#' qmplot(lon, lat, data = mg, shape = I(15), zoom = 14, legend = "topleft") +
#'   geom_point(aes(size = zinc), data = m, color = "green") +
#'   scale_size("Zinc (ppm)")
#'
#'
#'
#' # interpolate at unobserved locations (i.e. at meuse.grid points)
#' # pre-define scale for consistency
#' scale <- scale_color_gradient("Predicted\nZinc (ppm)",
#'   low = "green", high = "red", lim = c(100, 1850)
#' )
#'
#'
#'
#' # inverse distance weighting
#' idw <- idw(log(zinc) ~ 1, meuse, meuse.grid, idp = 2.5)
#' mg$idw <- exp(slot(idw, "data")$var1.pred)
#'
#' qmplot(lon, lat, data = mg, shape = I(15), color = idw,
#'   zoom = 14, legend = "topleft", alpha = I(.75), darken = .4
#' ) + scale
#'
#'
#'
#' # linear regression
#' lin <- krige(log(zinc) ~ 1, meuse, meuse.grid, degree = 1)
#' mg$lin <- exp(slot(idw, "lin")$var1.pred)
#'
#' qmplot(lon, lat, data = mg, shape = I(15), color = lin,
#'   zoom = 14, legend = "topleft", alpha = I(.75), darken = .4
#' ) + scale
#'
#'
#'
#' # trend surface analysis
#' tsa <- krige(log(zinc) ~ 1, meuse, meuse.grid, degree = 2)
#' mg$tsa <- exp(slot(tsa, "data")$var1.pred)
#'
#' qmplot(lon, lat, data = mg, shape = I(15), color = tsa,
#'   zoom = 14, legend = "topleft", alpha = I(.75), darken = .4
#' ) + scale
#'
#'
#'
#' # ordinary kriging
#' vgram <- variogram(log(zinc) ~ 1, meuse)   # plot(vgram)
#' vgramFit <- fit.variogram(vgram, vgm(1, "Exp", .2, .1))
#' ordKrige <- krige(log(zinc) ~ 1, meuse, meuse.grid, vgramFit)
#' mg$ordKrige <- exp(slot(ordKrige, "data")$var1.pred)
#'
#' qmplot(lon, lat, data = mg, shape = I(15), color = ordKrige,
#'   zoom = 14, legend = "topleft", alpha = I(.75), darken = .4
#' ) + scale
#'
#'
#'
#' # universal kriging
#' vgram <- variogram(log(zinc) ~ 1, meuse) # plot(vgram)
#' vgramFit <- fit.variogram(vgram, vgm(1, "Exp", .2, .1))
#' univKrige <- krige(log(zinc) ~ sqrt(dist), meuse, meuse.grid, vgramFit)
#' mg$univKrige <- exp(slot(univKrige, "data")$var1.pred)
#'
#' qmplot(lon, lat, data = mg, shape = I(15), color = univKrige,
#'   zoom = 14, legend = "topleft", alpha = I(.75), darken = .4
#' ) + scale
#'
#'
#'
#' # adding observed data layer
#' qmplot(lon, lat, data = mg, shape = I(15), color = univKrige,
#'   zoom = 14, legend = "topleft", alpha = I(.75), darken = .4
#' ) +
#'   geom_point(
#'     aes(x = lon, y = lat, size = zinc),
#'     data = m, shape = 1, color = "black"
#'   ) +
#'   scale +
#'   scale_size("Observed\nLog Zinc")
#'
#'
#'
#'
#'
#'
#' } # end dontrun
#'
qmplot <- function(x, y, ..., data, zoom, source = "stamen", maptype = "toner-lite",
  extent = "device", legend = "right", padding = .02, force = FALSE,
  darken = c(0, "black"), mapcolor = "color",
  facets = NULL, margins = FALSE, geom = "auto", stat = list(NULL),
  position = list(NULL), xlim = c(NA, NA), ylim = c(NA, NA), main = NULL, f = 0.05,
  xlab = "Longitude", ylab = "Latitude")
{


  if (!missing(stat))
    warning("`stat` is deprecated", call. = FALSE)
  if (!missing(position))
    warning("`position` is deprecated", call. = FALSE)
  if (!is.character(geom))
    stop("`geom` must be a character vector", call. = FALSE)

  argnames <- names(as.list(match.call(expand.dots=FALSE)[-1]))
  arguments <- as.list(match.call()[-1])
  env <- parent.frame()

  args <- as.list(match.call(expand.dots = TRUE)[-1])
  argsgiven <- names(args)
  if("mapcolour" %in% argsgiven) mapcolor <- eval(args$mapcolour)

  .all_aesthetics <- unlist(.all_aesthetics[1:42])
  aesthetics <- compact(arguments[.all_aesthetics])
  aesthetics <- aesthetics[!is.constant(aesthetics)]
  aes_names <- names(aesthetics)
  aesthetics <- rename_aes(aesthetics)
  class(aesthetics) <- "uneval"

  if (missing(data)) {
    # If data not explicitly specified, will be pulled from workspace
    data <- data.frame()

    # Faceting variables must be in a data frame, so pull those out
    facetvars <- all.vars(facets)
    facetvars <- facetvars[facetvars != "."]
    names(facetvars) <- facetvars
    facetsdf <- as.data.frame(mget(facetvars, envir = env))
    if (nrow(facetsdf)) data <- facetsdf
  }

  # Work out plot data, and modify aesthetics, if necessary
  if ("auto" %in% geom) {
    if ("sample" %in% aes_names) {
      geom[geom == "auto"] <- "qq"
    } else if (missing(y)) {
      x <- eval(aesthetics$x, data, env)
      if (is.discrete(x)) {
        geom[geom == "auto"] <- "bar"
      } else {
        geom[geom == "auto"] <- "histogram"
      }
      if (missing(ylab)) ylab <- "count"
    } else {
      if (missing(x)) {
        aesthetics$x <- bquote(seq_along(.(y)), aesthetics)
      }
      geom[geom == "auto"] <- "point"
    }
  }



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
      scale_x_continuous(limits = c(xmin, xmax), expand = c(0,0)) +
      scale_y_continuous(limits = c(ymin, ymax), expand = c(0,0))
  } else if(extent == "device"){
  	p <- p +
      scale_x_continuous(limits = c(xmin, xmax), expand = c(0,0)) +
      scale_y_continuous(limits = c(ymin, ymax), expand = c(0,0)) +
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

  if (!is.null(main)) p <- p + ggtitle(main)

  # Add geoms/statistics
  if (is.proto(position)) position <- list(position)

  # Add geoms/statistics
  for (g in geom) {
    # Arguments are unevaluated because some are aesthetics. Need to evaluate
    # params - can't do in correct env because that's lost (no lazyeval)
    # so do the best we can by evaluating in parent frame.
    params <- arguments[setdiff(names(arguments), c(aes_names, argnames))]
    params <- lapply(params, eval, parent.frame())

    p <- p + do.call(paste0("geom_", g), params)
  }

  if (!missing(xlab)) p <- p + xlab(xlab)
  if (!missing(ylab)) p <- p + ylab(ylab)

  if (!missing(xlim)) p <- p + xlim(xlim)
  if (!missing(ylim)) p <- p + ylim(ylim)

  p



  p
}
