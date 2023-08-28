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
#'\dontrun{ requires a Stadia Maps API key. see ?register_stadiamaps
#'
#' get_stadiamap(zoom = 9)
#'
#'}
#'
print.ggmap <- function(x, ...){

  source <- switch(attr(x, "source"),
    "google" = "Google Maps",
    "stadia" = "Stadia Maps",
    "osm" = "OpenStreetMap",
    "cloudmade" = "Cloudmade",
    "naver" = "Naver Map"
  )

  cli::cli_text("{nrow(x)}x{ncol(x)} {attr(x, 'maptype')} map image from {source}; \\
                use {.fn ggmap::ggmap} to plot it.")

  invisible(x)
}

