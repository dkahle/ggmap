.onAttach <- function(...) {
  if (!interactive()) return()

  packageStartupMessage("Google Maps API Terms of Service: http://developers.google.com/maps/terms.")
  packageStartupMessage("Please cite ggmap if you use it: see citation(\"ggmap\") for details.")

  # define initial credentials for ggmap
  if ( is.null(getOption("ggmap")) ) {

    options <- list(
      google = list(
        key = NA,
        account_type = "standard",
        client = NA,
        signature = NA
      )
    )
    class(options) <- "ggmap_credentials"
    options(ggmap = options)

    if (goog_account() == "premium") {
      register_google(second_limit = 50, day_limit = 2500)
    } else {
      register_google(second_limit = 50, day_limit = 100000)
    }
  }

}







