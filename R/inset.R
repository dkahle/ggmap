#' Add ggplot2 insets to a map
#'
#' This is identical to ggplot2::annotation_custom for use with ggmap
#'
#' Most useful for adding tables, inset plots, and other grid-based decorations
#'
#' @param grob grob to display
#' @param xmin,xmax x location (in data coordinates) giving horizontal
#'   location of raster
#' @param ymin,ymax y location (in data coordinates) giving vertical
#'   location of raster
#' @export inset
#' @note \code{annotation_custom} expects the grob to fill the entire viewport
#' defined by xmin, xmax, ymin, ymax. Grobs with a different (absolute) size 
#' will be center-justified in that region.
#' Inf values can be used to fill the full plot panel 
inset <- annotation_custom <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) { 
  GeomCustomAnn$new(geom_params = list(grob = grob, xmin = xmin, 
    xmax = xmax, ymin = ymin, ymax = ymax), stat = "identity", 
    position = "identity", data = NULL, inherit.aes = TRUE)
}

GeomCustomAnn <- proto(ggplot2:::Geom, {
   objname <- "custom_ann"
  
  draw_groups <- function(., data, scales, coordinates, grob, xmin, xmax,
                          ymin, ymax, ...) {
    #if (!inherits(coordinates, "cartesian")) {
    #  stop("annotation_custom only works with Cartesian coordinates", 
    #    call. = FALSE)
    #}

    if(is.infinite(xmin)) xmin <- scales$x.range[1]
    if(is.infinite(xmax)) xmax <- scales$x.range[2]
    if(is.infinite(ymin)) ymin <- scales$y.range[1]
    if(is.infinite(ymax)) ymax <- scales$y.range[2]           
    
    corners <- data.frame(x = c(xmin, xmax), y = c(ymin, ymax))
    data <- coord_transform(coordinates, corners, scales)

    x_rng <- range(data$x, na.rm = TRUE)
    y_rng <- range(data$y, na.rm = TRUE)

    vp <- viewport(x = mean(x_rng), y = mean(y_rng),
                   width = diff(x_rng), height = diff(y_rng),
                   just = c("center","center"))
    editGrob(grob, vp = vp)
  }
  
  default_aes <- function(.) 
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)  
})





