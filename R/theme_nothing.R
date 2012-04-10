#' Make a blank ggplot2 theme.
#'
#' theme_nothing simply strips all thematic element in ggplot2 for map plotting.
#' 
#' @param base_size base size, not used.
#' @return a ggplot2 theme (i.e., a list of class options).
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @export
#' @examples
#' library(ggplot2)
#' \dontrun{
#'
#' n <- 50
#' df <- expand.grid(x = 1:n,y = 1:n)[sample(n^2,.5*n^2),]
#' qplot(x, y, data = df, geom = 'tile')
#' qplot(x, y, data = df, geom = 'tile') + theme_nothing()
#' qplot(x, y, data = df, geom = 'tile') + 
#'   scale_x_continuous(expand = c(0,0)) +
#'   scale_y_continuous(expand = c(0,0)) +
#'   theme_nothing()
#' 
#' qplot(1:10,1:10) +
#'   theme_nothing()  +
#'   opts(panel.background = theme_rect(fill = 'black'))
#'
#' }
#'
theme_nothing <- function (base_size = 12){
  structure(list(
    axis.line = theme_blank(), 
    axis.text.x = theme_blank(), 
    axis.text.y = theme_blank(),
    axis.ticks = theme_blank(), 
    axis.title.x = theme_blank(), 
    axis.title.y = theme_blank(), 
    axis.ticks.length = unit(0, "cm"), 
    axis.ticks.margin = unit(0, "cm"), 
    legend.position = "none", 
    panel.background = theme_blank(), 
    panel.border = theme_blank(), 
    panel.grid.major = theme_blank(), 
    panel.grid.minor = theme_blank(), 
    panel.margin = unit(0, "lines"), 
    plot.background = theme_blank(), 
    plot.margin = unit(c(0, 0, -.5, -.5), "lines")
  ), class = "options")
}