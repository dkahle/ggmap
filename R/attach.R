.onAttach <- function(...) {
  # if (!interactive()) return()

  packageStartupMessage("Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.")
  packageStartupMessage("Please cite ggmap if you use it! See citation(\"ggmap\") for details.")

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








