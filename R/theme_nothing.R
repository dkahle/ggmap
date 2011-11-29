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
#' df <- expand.grid(x = 1:100,y = 1:100)[sample(100^2,.75*100^2),]
#' qplot(x, y, data = df, geom = 'tile')
#' qplot(x, y, data = df, geom = 'tile') + theme_nothing()
theme_nothing <- function (base_size = 12){
  structure(list(
    axis.line = theme_blank(), 
    axis.text.x = theme_blank(), 
    axis.text.y = theme_blank(),
    axis.ticks = theme_blank(), 
    axis.title.x = theme_blank(), 
    axis.title.y = theme_blank(), 
    axis.ticks.length = unit(0, "lines"), 
    axis.ticks.margin = unit(0, "lines"), 
    legend.position = "none", 
    panel.background = theme_blank(), 
    panel.border = theme_blank(), 
    panel.grid.major = theme_blank(), 
    panel.grid.minor = theme_blank(), 
    panel.margin = unit(0, "lines"), 
    plot.background = theme_blank(), 
    plot.margin = unit(c(-1.5, -1.5, -1.5, -1.5), "lines")
  ), class = "options")
}