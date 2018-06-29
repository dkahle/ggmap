#' Make a blank ggplot2 theme.
#'
#' theme_nothing simply strips all thematic element in ggplot2.
#'
#' @param base_size base size, not used.
#' @param legend should the legend be included?
#' @return a ggplot2 theme (i.e., a list of class options).
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @export
#' @examples
#'
#'
#' # no legend example
#' n <- 50
#' df <- expand.grid(x = 1:n,y = 1:n)[sample(n^2,.5*n^2),]
#' p <- ggplot(df, aes(x, y)) + geom_raster()
#' p
#' p + theme_nothing()
#' p + theme_nothing(legend = TRUE) # no difference
#' p +
#'   scale_x_continuous(expand = c(0,0)) +
#'   scale_y_continuous(expand = c(0,0)) +
#'   theme_nothing()
#'
#'
#'
#' # legend example
#' df$class <- factor(sample(0:1, .5*n^2,  replace = TRUE))
#' p <- ggplot(df, aes(x, y)) + geom_raster(aes(fill = class))
#' p
#' p + theme_nothing()
#' p + theme_nothing(legend = TRUE)
#'
#' p <- p +
#'   scale_x_continuous(expand = c(0,0)) +
#'   scale_y_continuous(expand = c(0,0))
#' p
#' p + theme_nothing()
#' p + theme_nothing(legend = TRUE)
#'
#'
#'
theme_nothing <- function(base_size = 12, legend = FALSE){
  if(legend){
    theme(
      axis.text =          element_blank(),
      axis.title =         element_blank(),
      panel.background =   element_blank(),
      panel.grid.major =   element_blank(),
      panel.grid.minor =   element_blank(),
      axis.ticks.length =  unit(0, "cm"),
      panel.spacing =      unit(0, "lines"),
      plot.margin =        unit(c(0, 0, 0, 0), "lines")
    )
  } else {
    theme(
      line =               element_blank(),
      rect =               element_blank(),
      text =               element_blank(),
      axis.ticks.length =  unit(0, "cm"),
      legend.position =    "none",
      panel.spacing =      unit(0, "lines"),
      plot.margin =        unit(c(0, 0, 0, 0), "lines")
    )
  }
}
