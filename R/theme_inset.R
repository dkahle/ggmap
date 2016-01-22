#' Make a ggplot2 inset theme.
#'
#' theme_inset is a ggplot2 theme geared towards making inset plots.
#'
#' @param base_size base size, not used.
#' @return a ggplot2 theme (i.e., a list of class options).
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @export
#' @examples
#' library(ggplot2)
#' \dontrun{
#'
#'
#' n <- 50
#' df <- expand.grid(x = 1:n,y = 1:n)[sample(n^2,.5*n^2),]
#' qplot(x, y, data = df, geom = 'tile')
#' qplot(x, y, data = df, geom = 'tile') + theme_nothing()
#'
#' qplot(1:10, 1:10) +
#'   annotation_custom(
#'     grob = ggplotGrob(qplot(1:10,1:10)),
#'     8, Inf, -Inf, 2
#'   )
#'
#' qplot(1:10, 1:10) +
#'   annotation_custom(
#'     grob = ggplotGrob(qplot(1:10,1:10) + theme_nothing()),
#'     8, Inf, -Inf, 2
#'   )
#'
#' qplot(1:10, 1:10) +
#'   annotation_custom(
#'     grob = ggplotGrob(qplot(1:10,1:10) + theme_inset()),
#'     8, Inf, -Inf, 2
#'   )
#'
#' }
#'
theme_inset <- function (base_size = 12){
  t <- theme_get()
  t$axis.line <- element_blank()
  t$axis.text.x <- element_blank()
  t$axis.text.y <- element_blank()
  t$axis.ticks <- element_blank()
  t$axis.title.x <- element_blank()
  t$axis.title.y <- element_blank()
  t$axis.ticks.length <- unit(0, "lines")
  t$axis.ticks.margin <- unit(0, "lines")
  t$panel.margin <- unit(0, "lines")
  t$plot.margin <- unit(c(0, 0, 0, 0), "lines")
  t$legend.position <- 'none'
  t
}
