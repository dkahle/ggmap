.onAttach <- function(...) {
  if (!interactive()) return()

  tips <- c(
    "Google Maps API Terms of Service : http://developers.google.com/maps/terms"  
  )  
  
  tip <- sample(tips, 1)
  packageStartupMessage(tip)
}