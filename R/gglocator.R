#' Locator for ggplots.
#'
#' Locator for ggplots. (Note : only accurate when extent = "normal"
#' when using ggmap.)
#'
#' @param n number of points to locate.
#' @param message turn messaging from grid.ls on/off
#' @param xexpand expand argument in scale_x_continuous
#' @param yexpand expand argument in scale_y_continuous
#' @return a data frame with columns according to the x and y
#'   aesthetics
#' @author Tyler Rinker with help from Baptiste Auguie and
#'   StackOverflow user DWin with additions and canning by David
#'   Kahle \email{david.kahle@@gmail.com}. Updated by
#'   \@Nikolai-Hlubek
#' @export
#' @examples
#'
#' if(interactive()){
#'
#' # only run for interactive sessions
#'
#'
#' df <- expand.grid(x = 0:-5, y = 0:-5)
#' (p <- qplot(x, y, data = df) +
#'   annotate(geom = 'point', x = -2, y = -2, colour = 'red'))
#' gglocator()
#'
#' p +
#'   scale_x_continuous(expand = c(0,0)) +
#'   scale_y_continuous(expand = c(0,0))
#' gglocator(1, xexpand = c(0,0), yexpand = c(0,0))
#'
#'
#' }
#'
#'
gglocator <- function(n = 1, message = FALSE,
  xexpand = c(.05, 0), yexpand = c(.05, 0)
){

  if(n > 1){
    df <- NULL
    for(k in 1:n){
      df <- rbind(df, gglocator(message = message,
        xexpand = xexpand, yexpand = yexpand))
    }
    return(df)
  }

  # find the correct viewport for the npc coordinates
  x <- grid.ls(print = message)$name
  x <- x[grep("panel.", x)][1]
  seekViewport(x)

  # get the position relative to that viewport
  loc <-  as.numeric(grid.locator("npc"))

  # scale the position to the plot
  object <- last_plot()
  # get the x.range and y.range from ggplot
  plot_info <- ggplot_build(object)
  xrng <- plot_info$panel$ranges[[1]]$x.range
  yrng <- plot_info$panel$ranges[[1]]$y.range

  xrng <- expand_range(range = xrng, mul = xexpand[1], add = xexpand[2])
  yrng <- expand_range(range = yrng, mul = yexpand[1], add = yexpand[2])

  # format and return
  point <- data.frame(xrng[1] + loc[1]*diff(xrng), yrng[1] + loc[2]*diff(yrng))
  names(point) <- with(object, c(deparse(mapping$x), deparse(mapping$y)))
  point
}
