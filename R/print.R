#' Print a map
#'
#' Print a console description of a map
#'
#' @param x an object of class elicit
#' @param ... additional parameters
#' @usage \method{print}{ggmap}(x, ...)
#' @return Invisibly returns `x`.
#' @export
#' @examples
#'
#'
#' get_stamenmap(zoom = 9)
#'
#'
print.ggmap <- function(x, ...){

  source <- switch(attr(x, "source"),
    "google" = "Google Maps",
    "stamen" = "Stamen Maps",
    "osm" = "OpenStreetMap",
    "cloudmade" = "Cloudmade",
    "naver" = "Naver Map"
  )

  cli::cli_text("{nrow(x)}x{ncol(x)} {attr(x, 'maptype')} map image from {source}; \\
                use {.fn ggmap::ggmap} to plot it.")

  invisible(x)
}

