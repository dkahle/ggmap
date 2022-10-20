#' Locator for ggplot objects
#'
#' Locator for ggplot objects (Note : only accurate when extent = "normal" when
#' using ggmap.)
#'
#' @param n number of points to locate.
#' @param message unused
#' @param mercator logical flag; should the plot be treated as using the
#'   projection common to most web map services? Set to FALSE if the axes on the
#'   plot use a linear scale.
#' @param ... additional arguments (including deprecated, e.g. xexpand)
#' @return a data frame with columns according to the x and y aesthetics
#' @author Tyler Rinker, Baptiste Auguie, DWin, David Kahle, \@Nikolai-Hlubek
#'   and \@mvkorpel.
#' @export
#' @examples
#'
#' if (interactive()) {
#'
#' # only run for interactive sessions
#' df <- expand.grid(x = 0:-5, y = 0:-5)
#'
#' ggplot(df, aes(x, y)) + geom_point() +
#'   annotate(geom = "point", x = -2, y = -2, colour = "red")
#'
#' (pt <- gglocator(mercator = FALSE)) # click red point
#'
#' last_plot() +
#'   annotate("point", pt$x, pt$y, color = "blue", size = 3, alpha = .5)
#'
#' hdf <- get_map("houston, texas")
#' ggmap(hdf, extent = "normal")
#' (pt <- gglocator(mercator = TRUE))
#' last_plot() +
#'   annotate("point", pt$lon, pt$lat, color = "blue", size = 3, alpha = .5)
#'
#' }
#'
#'
gglocator <- function(n = 1, message = FALSE, mercator = TRUE, ...){

  if (
    .Platform$GUI == "RStudio" && str_detect(.Device, "(RStudio)|(null device)")
  ) {
    cli::cli_abort(
      "{.fn ggmap::gglocator} is unreliable in the RStudio plot viewer. Create a different device with {.fn grDevices::x11} or {.fn grDevices::quartz}."
    )
  }

  args <- as.list(match.call(expand.dots = FALSE))
  if ( "..." %in% names(args) && any(c("xexpand", "yexpand") %in% names(args$`...`)) ) {
    cli::cli_alert_warning("{.arg xexpand} and {.arg yexpand} are no longer used in {.fn ggmap::gglocator}.")
  }

  if (n > 1) {
    df <- NULL
    for (k in 1:n) {
      df <- rbind(df, gglocator(message = message, mercator = mercator, ...))
    }
    return(df)
  }

  object <- last_plot()
  if(is.null(object)) cli::cli_abort("No plots available.")

  # find the correct viewport for the npc coordinates
  x <- unlist(current.vpTree())
  x <- unname(x[grep("\\.name$", names(x))])
  x <- grep("panel", x, fixed = TRUE, value = TRUE)
  n_panels <- length(x)
  if (n_panels == 0) cli::cli_abort("{.pkg ggplot2} graphic not detected in current device.")
  if (n_panels > 1) {
    x <- x[1]
    cli::cli_alert_warning("Multiple plots detected, using the first one (\"{x}\")")
  }
  previous_viewport <- current.vpPath()
  seekViewport(x, recording = FALSE)

  # when exiting function, return to previous position in viewport tree
  on.exit(upViewport(0, recording = FALSE))
  if (!is.null(previous_viewport)) {
    on.exit(downViewport(previous_viewport, strict = TRUE, recording = FALSE), add = TRUE)
  }

  # get the position relative to that viewport
  loc <- as.numeric(grid.locator("npc"))

  # scale the position to the plot

  # get the x.range and y.range from ggplot
  plot_info <- ggplot_build(object)
  if ("layout" %in% names(plot_info)) {
    xrng <- plot_info$layout$panel_params[[1]]$x.range
    yrng <- plot_info$layout$panel_params[[1]]$y.range
  } else {
    cli::cli_abort("No plot layout.")
  }

  # format and return
  point <- data.frame(
    xrng[1] + loc[1]*diff(xrng),
    yrng[1] + loc[2]*diff(yrng)
  )

  if(isTRUE(mercator)){
    yrng2 <- LonLat2XY(0, yrng, zoom = 0, ypix = 256)$y
    point[[2]] <- XY2LonLat(
      x = 0, y = yrng2[1] + loc[2] * diff(yrng2),
      X = 0, Y = 0,
      zoom = 0, ypix = 256)[[2]]
  }

  names(point) <- c(object$labels$x, object$labels$y)
  point
}
