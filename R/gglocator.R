#' Locator for ggplots.
#'
#' Locator for ggplots. (Note : only accurate when extent = "normal"
#' when using ggmap.)
#'
#' @param n number of points to locate.
#' @param message unused
#' @param xexpand expand argument in scale_x_continuous
#' @param yexpand expand argument in scale_y_continuous
#' @param mercator logical flag; should the plot be treated as using
#'   the projection common to most web map services? Set to FALSE if
#'   the axes on the plot use a linear scale.
#' @return a data frame with columns according to the x and y
#'   aesthetics
#' @author Tyler Rinker with help from Baptiste Auguie and
#'   StackOverflow user DWin with additions and canning by David
#'   Kahle \email{david.kahle@@gmail.com}. Updated by
#'   \@Nikolai-Hlubek and \@mvkorpel.
#' @export
#' @examples
#'
#' if(interactive()){
#'
#' # only run for interactive sessions
#' df <- expand.grid(x = 0:-5, y = 0:-5)
#' p <- qplot(x, y, data = df) +
#'   annotate(geom = "point", x = -2, y = -2, colour = "red")
#' print(p)
#' cat("click red point\n")
#' print(pt <- gglocator(mercator = FALSE))
#' p2 <- last_plot() +
#'   annotate("point", pt$x, pt$y, color = "blue", size = 3, alpha = .5)
#' cat("a blue point should appear where you clicked\n")
#' print(p2)
#'
#' p3 <- p +
#'   scale_x_continuous(expand = c(0,0)) +
#'   scale_y_continuous(expand = c(0,0))
#' print(p3)
#' cat("click any point\n")
#' print(gglocator(1, xexpand = c(0,0), yexpand = c(0,0),
#'                 mercator = FALSE))
#'
#'
#' }
#'
#'
gglocator <- function(n = 1, message = FALSE,
  xexpand = c(.0, 0), yexpand = c(.0, 0), mercator = TRUE
){

  if(n > 1){
    df <- NULL
    for(k in 1:n){
      df <- rbind(df, gglocator(message = message,
        xexpand = xexpand, yexpand = yexpand, mercator = mercator))
    }
    return(df)
  }

  object <- last_plot()
  if(is.null(object)){
    stop("no plots available")
  }

  # find the correct viewport for the npc coordinates
  x <- unlist(current.vpTree())
  x <- unname(x[grep("\\.name$", names(x))])
  x <- grep("panel", x, fixed = TRUE, value = TRUE)
  n_panels <- length(x)
  if(n_panels == 0){
    stop("ggmap plot not detected in current device")
  }
  if(n_panels > 1){
    x <- x[1]
    warning(gettextf("multiple plots detected, choosing one (\"%s\")",
                     x), domain = NA)
  }
  previous_viewport <- current.vpPath()
  seekViewport(x, recording = FALSE)

  # when exiting function, return to previous position in viewport tree
  on.exit(upViewport(0, recording = FALSE))
  if(!is.null(previous_viewport)){
    on.exit(downViewport(previous_viewport, strict = TRUE, recording = FALSE),
            add = TRUE)
  }

  # get the position relative to that viewport
  loc <-  as.numeric(grid.locator("npc"))

  # scale the position to the plot

  # get the x.range and y.range from ggplot
  plot_info <- ggplot_build(object)
  if("layout" %in% names(plot_info)){
    ranges <- plot_info$layout$panel_ranges[[1]]
  } else{
    ranges <- plot_info$panel$ranges[[1]]
  }
  xrng <- ranges$x.range
  yrng <- ranges$y.range

  xrng <- expand_range(range = xrng, mul = xexpand[1], add = xexpand[2])
  yrng <- expand_range(range = yrng, mul = yexpand[1], add = yexpand[2])

  # format and return
  point <- data.frame(xrng[1] + loc[1]*diff(xrng),
                      yrng[1] + loc[2]*diff(yrng))
  if(isTRUE(mercator)){
    yrng2 <- LonLat2XY(0, yrng, zoom = 0, ypix = 256)$y
    point[[2]] <- XY2LonLat(y = yrng2[1] + loc[2] * diff(yrng2),
                            x = 0, X = 0, Y = 0, zoom = 0, ypix = 256)[[2]]
  }
  names(point) <- with(object, c(deparse(mapping$x), deparse(mapping$y)))
  point
}
