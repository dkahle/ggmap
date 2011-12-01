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
