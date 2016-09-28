#' Reverse geocode
#'
#' reverse geocodes a longitude/latitude location using Google Maps.
#' Note that in most cases by using this function you are agreeing
#' to the Google Maps API Terms of Service at
#' https://developers.google.com/maps/terms.
#'
#' @param location a location in longitude/latitude format
#' @param output amount of output
#' @param messaging turn messaging on/off
#' @param urlonly return only the url?
#' @param override_limit override the current query count
#'   (.GoogleGeocodeQueryCount)
#' @param ext domain extension (e.g. "com", "co.nz")
#' @param inject character string to add to the url
#' @param ... ...
#' @return depends (at least an address)
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso
#' \url{http://code.google.com/apis/maps/documentation/geocoding/}
#' @export
#' @examples
#'
#' \dontrun{ # Server response can be slow; this cuts down check time.
#'
#' ( gc <- as.numeric(geocode("the white house")) )
#' revgeocode(gc)
#' revgeocode(gc, output = "more")
#' revgeocode(gc, output = "all")
#' geocodeQueryCheck()
#'
#' }
#'
revgeocode <- function(location, output = c("address","more","all"),
  messaging = FALSE, urlonly = FALSE, override_limit = FALSE,
  ext = "com", inject = "", ...
){

  # check parameters
  stopifnot(is.numeric(location) && length(location) == 2)
  output <- match.arg(output)
  stopifnot(is.logical(messaging))


  # format url
  latlng <- paste(rev(location), collapse = ',')
  posturl <- fmteq(latlng)
  url_string <- paste0(
    sprintf("https://maps.googleapis.%s/maps/api/geocode/json?", ext),
    posturl
  )

  # do google credentials
  if (has_goog_client() && has_goog_signature()) {
    client <- goog_client()
    signature <- goog_signature()
    url_string <- paste(url_string, fmteq(client), fmteq(signature), sep = "&")
  } else if (has_goog_key()) {
    key <- goog_key()
    url_string <- paste(url_string, fmteq(key), sep = "&")
  }

  # inject any remaining stuff
  if(inject != "") url_string <- paste(url_string, inject, sep = "&")

  # encode
  url_string <- URLencode( enc2utf8(url_string) )
  if(urlonly) return(url_string)

  # check/update google query limit
  check <- checkGeocodeQueryLimit(url_string, elems = 1, override = override_limit, messaging = messaging)

  if(check == "stop"){
    if(output == "address"){
      return(NA)
    } else if(output == "more") {
      return(c(address = NA, street_number = NA, route = NA,
        locality = NA, administrative_area_level_2 = NA,
        administrative_area_level_1 = NA, country = NA, postal_code = NA)
      )
    } else {
      return(NA)
    }
  }

  # geocode
  connect <- url(url_string); on.exit(close(connect), add = TRUE)
  rgc <- fromJSON(paste(readLines(connect), collapse = ''))
  if(output == "all") return(rgc)

  # did geocode fail?
  if(rgc$status != 'OK'){
    warning(paste('reverse geocode failed - bad location? location = "',
      location, '"', sep = ''))
    return(data.frame(address = NA))
  }

  # message user
  message(paste0('Information from URL : ', url_string))

  # more than one location found?
  if(length(rgc$results) > 1 && messaging){
    message("more than one location found for \"", location, "\", reverse geocoding first...\n")
  }

  # format
  rgc <- rgc$results[[1]]
  if(output == 'address') return(rgc$formatted_address)

  with(rgc, {
    rgcdf <<- data.frame(address = formatted_address)
  })

  for(k in seq_along(rgc$address_components)){
  	rgcdf <- cbind(rgcdf, rgc$address_components[[k]]$long_name)
  }
  names(rgcdf) <- c("address", sapply(rgc$address_components, function(l) l$types[1]))

  # return 'more' output
  rgcdf
}
