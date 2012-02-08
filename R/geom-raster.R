#' geom-raster
#'
#' a special version of geom_raster (ggplot2) for use in ggmap.
#' 
#' @param mapping The aesthetic mapping, usually constructed with
#'    \code{\link{aes}} or \code{\link{aes_string}}. Only needs to be set
#'    at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'    the plot defaults.
#' @param stat The statistical transformation to use on the data for this
#'    layer. 
#' @param position The position adjustment to use for overlappling points
#'    on this layer
#' @param image raster image
#' @param interpolate whether to interpolate.  see ?grid::rasterGrob
#' @param ... other arguments passed on to \code{\link{layer}}. This can 
#'   include aesthetics whose values you want to set, not map. See
#'   \code{\link{layer}} for more details.
#' @return a named numeric vector of geographical coordinates.
#' @author Hadley Wickham with additions by Kohske Takahashi
#' @examples
#' NULL
#'
#' 
geom_raster <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", image = NULL, interpolate = TRUE, ...) { 
  GeomRaster$new(mapping = mapping, data = data, stat = stat, position = position, image = image, interpolate = interpolate, ...)
}

GeomRaster <- proto(ggplot2:::Geom, {
  objname <- "raster"
  
  default_stat <- function(.) StatIdentity
  default_pos <- function(.) PositionIdentity
  default_aes <- function(.) aes()
  
  required_aes <- c("xmin", "xmax", "ymin", "ymax")

  draw <- draw_groups <- function(., data, scales, coordinates, image = image, interpolate = interpolate, ...) {
    with(coord_transform(coordinates, data, scales), 
      ggname(.$my_name(), rasterGrob(
        image,
        xmin, ymax,
        width = xmax - xmin, height = ymax - ymin, 
        interpolate = interpolate,
        default.units = "native", just = c("left", "top"), 
        ))
      )
  }
                  
  guide_geom <- function(.) "polygon"
})