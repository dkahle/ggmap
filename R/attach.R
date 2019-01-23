.onAttach <- function(...) {
  # if (!interactive()) return()

  packageStartupMessage("Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.")
  packageStartupMessage("Please cite ggmap if you use it; see citation(\"ggmap\") for details.")

  set_ggmap_option(
    "google" = structure(
      list(
        # "key" = NA, # now stored as an environment variable
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








