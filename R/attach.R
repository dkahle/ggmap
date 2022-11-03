.onAttach <- function(...) {

  cli::cli_alert_info("Google's Terms of Service: {.url https://mapsplatform.google.com}")
  cli::cli_alert_info("Please cite {.pkg ggmap} if you use it! Use {.code citation(\"ggmap\")} for details.")

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








