.onAttach <- function(...) {
  if (!interactive()) return()

  packageStartupMessage("Google Maps API Terms of Service : http://developers.google.com/maps/terms")  
  packageStartupMessage('ggmap is made available free of charge.  if you use it, please cite it!')  
  packageStartupMessage('type citation("ggmap") for details how.')    
  

}