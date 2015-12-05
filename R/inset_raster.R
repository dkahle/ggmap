#' Create a (ggplot2) raster layer
#'
#' This is a special version of ggplot2::annotation_raster for use
#' with ggmap.  (It simply removes the requirement for cartesian
#' coordinates.)  The only difference between \code{inset_raster}
#' and \code{inset_ggmap} is their arguments.  \code{inset_ggmap} is
#' simply a wrapper of \code{inset_raster} with \code{xmin}, ...,
#' \code{ymax} arguments equal to the map's bounding box.
#'
#' @param raster raster object to display
#' @param xmin,xmax x location (in data coordinates) giving
#'   horizontal location of raster
#' @param ymin,ymax y location (in data coordinates) giving vertical
#'   location of raster
#' @param interpolate interpolate the raster? (i.e. antialiasing)
#' @param ggmap a ggmap object, see \code{\link{get_map}}
#' @export
#' @name inset_raster
#' @examples
#'
#' # see ?bb2bbox
#'
#'
#'
inset_raster <- annotation_raster <- function(raster, xmin, xmax, ymin, ymax, interpolate = TRUE) {
  raster <- grDevices::as.raster(raster)

  # add possibility that xmin is c(xmin, xmax, ymin, ymax)
  if(length(xmin) == 4 && missing(xmax)){
    xmax <- xmin[2]
    ymin <- xmin[3]
    ymax <- xmin[4]
    xmin <- xmin[1]
  }

  layer(
    data = NULL,
    mapping = NULL,
    stat = ggplot2::StatIdentity,
    position = ggplot2::PositionIdentity,
    geom = GeomRasterAnn,
    inherit.aes = TRUE,
    params = list(
      raster = raster,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      interpolate = interpolate
    )
  )

}


#' @rdname inset_raster
#' @export
inset_ggmap <- function(ggmap){
  stopifnot(inherits(ggmap, "ggmap"))
  bb <- attr(ggmap, "bb")
  bbox <- bb2bbox(bb)

  inset_raster(ggmap, bbox[c("left", "right", "bottom", "top")])
}



GeomRasterAnn <- ggproto("GeomRasterAnn", Geom,
  extra_params = "",
  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, panel_scales, coord, raster, xmin, xmax,
                        ymin, ymax, interpolate = FALSE) {
#     if (!inherits(coord, "CoordCartesian")) {
#       stop("annotation_raster only works with Cartesian coordinates",
#         call. = FALSE)
#     }
    corners <- data.frame(x = c(xmin, xmax), y = c(ymin, ymax))
    data <- coord$transform(corners, panel_scales)

    x_rng <- range(data$x, na.rm = TRUE)
    y_rng <- range(data$y, na.rm = TRUE)

    rasterGrob(raster, x_rng[1], y_rng[1],
      diff(x_rng), diff(y_rng), default.units = "native",
      just = c("left","bottom"), interpolate = interpolate)
  }
)
