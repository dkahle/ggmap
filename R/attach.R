.onAttach <- function(...) {
  # if (!interactive()) return()

  packageStartupMessage("Google Maps API Terms of Service: https://cloud.google.com/maps-platform/terms/.")
  packageStartupMessage("Please cite ggmap if you use it: see citation(\"ggmap\") for details.")

  # define initial credentials for ggmap
  # if ( is.null(getOption("ggmap")) ) {
  #
  #   options <- list(
  #     "google" = list(
  #       key = NA,
  #       account_type = "standard",
  #       day_limit = NA, #2500,
  #       second_limit = NA, #50,
  #       client = NA,
  #       signature = NA
  #     ),
  #     "print_api_key" = FALSE
  #   )
  #   class(options) <- "ggmap_credentials"
  #   options(ggmap = options)
  #
  # }

  set_ggmap_option(
    "google" = structure(
      list(
        "key" = NA,
        "account_type" = "standard",
        "day_limit" = Inf, #2500,
        "second_limit" = 50L,
        "client" = NA,
        "signature" = NA
      ),
      class = "google_credentials"
    ),
    "display_api_key" = FALSE
  )


}





ggmap_environment <- new.env(parent = emptyenv())








