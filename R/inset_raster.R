#' Add a raster annotation to a map
#'
#' This is a special version of ggplot2::annotation_raster for use with ggmap
#'
#' Most useful for adding bitmap images
#'
#' @param raster raster object to display
#' @param xmin,xmax x location (in data coordinates) giving horizontal
#'   location of raster
#' @param ymin,ymax y location (in data coordinates) giving vertical
#'   location of raster
#' @export inset_raster
inset_raster <- annotation_raster <- function (raster, xmin, xmax, ymin, ymax) { 
  raster <- as.raster(raster)
  GeomRasterAnn$new(geom_params = list(raster = raster, xmin = xmin, 
    xmax = xmax, ymin = ymin, ymax = ymax), stat = "identity", 
    position = "identity", data = NULL, inherit.aes = TRUE)
}

GeomRasterAnn <- proto(ggplot2:::GeomRaster, {
  objname <- "raster_ann"
  reparameterise <- function(., df, params) {
    df
  }

  
  draw_groups <- function(., data, scales, coordinates, raster, xmin, xmax,
    ymin, ymax, ...) {
    #if (!inherits(coordinates, "cartesian")) {
    #  stop("annotation_raster only works with Cartesian coordinates", 
    #    call. = FALSE)
    #}
    corners <- data.frame(x = c(xmin, xmax), y = c(ymin, ymax))
    data <- coord_transform(coordinates, corners, scales)

    x_rng <- range(data$x, na.rm = TRUE)
    y_rng <- range(data$y, na.rm = TRUE)
        
    rasterGrob(raster, x_rng[1], y_rng[1], 
      diff(x_rng), diff(y_rng), default.units = "native", 
      just = c("left","bottom"), interpolate = TRUE)
  }
})

