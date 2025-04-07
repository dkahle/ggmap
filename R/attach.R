.onAttach <- function(...) {

  tos <- paste0(
    cli::col_green(cli::symbol$info),
    " ",
    "Google's Terms of Service: ",
    cli::col_blue(cli::style_italic(
      cli::style_hyperlink("<https://mapsplatform.google.com>", "https://mapsplatform.google.com")
    )),
    "\n  ",
    "Stadia Maps' Terms of Service: ",
    cli::col_blue(cli::style_italic(
      cli::style_hyperlink("<https://stadiamaps.com/terms-of-service>", "https://stadiamaps.com/terms-of-service")
    )),
    "\n  ",
    "OpenStreetMap's Tile Usage Policy: ",
    cli::col_blue(cli::style_italic(
      cli::style_hyperlink("<https://operations.osmfoundation.org/policies/tiles>", "https://operations.osmfoundation.org/policies/tiles")
    ))
  )
  cite <- paste0(
    cli::col_green(cli::symbol$info),
    " ",
    "Please cite ", cli::col_blue("ggmap"), " if you use it! Use `citation(\"ggmap\")` for details."
  )

  rlang::inform(
    paste0(tos, "\n", cite),
    class = "packageStartupMessage"
  )

  bootstrap_ggmap()

}





bootstrap_ggmap <- function () {
  set_ggmap_option(
    "google" = structure(
      list(
        "account_type" = "standard",
        "day_limit" = Inf, #2500,
        "second_limit" = 50L
      ),
      class = "google_credentials"
    ),
    "display_api_key" = FALSE
  )
}



ggmap_environment <- new.env(parent = emptyenv())








