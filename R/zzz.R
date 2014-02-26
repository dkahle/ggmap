.onAttach <- function(...) {
  if (!interactive()) return()

  packageStartupMessage("Google Maps API Terms of Service : ")  
  packageStartupMessage('  http://developers.google.com/maps/terms')    
  packageStartupMessage('please cite ggmap if you use it.')  
  packageStartupMessage('type citation("ggmap") for details how.')    
  

}