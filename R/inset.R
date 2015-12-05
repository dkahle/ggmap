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
inset <- annotation_custom <- function(grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) {
  layer(
    data = NULL,
    stat = ggplot2::StatIdentity,
    position = ggplot2::PositionIdentity,
    geom = GeomCustomAnn,
    inherit.aes = TRUE,
    params = list(
      grob = grob,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    )
  )
}


GeomCustomAnn <- ggproto("GeomCustomAnn", Geom,
  extra_params = "",
  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, panel_scales, coord, grob, xmin, xmax,
                        ymin, ymax) {
#     if (!inherits(coord, "CoordCartesian")) {
#       stop("annotation_custom only works with Cartesian coordinates",
#         call. = FALSE)
#     }
    corners <- data.frame(x = c(xmin, xmax), y = c(ymin, ymax))
    data <- coord$transform(corners, panel_scales)

    x_rng <- range(data$x, na.rm = TRUE)
    y_rng <- range(data$y, na.rm = TRUE)

    vp <- viewport(x = mean(x_rng), y = mean(y_rng),
                   width = diff(x_rng), height = diff(y_rng),
                   just = c("center","center"))
    editGrob(grob, vp = vp, name = paste(grob$name, annotation_id()))
  },

  default_aes = aes_(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
)





annotation_id <- local({
  i <- 1
  function() {
    i <<- i + 1
    i
  }
})
