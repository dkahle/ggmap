.onAttach <- function(...) {
  if (!interactive()) return()

  packageStartupMessage("Google Maps API Terms of Service: http://developers.google.com/maps/terms.")
  packageStartupMessage("Please cite ggmap if you use it: see citation('ggmap') for details.")
}
