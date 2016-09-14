#' Geocode
#'
#' Geocodes a location (find latitude and longitude) using either
#' (1) the Data Science Toolkit
#' (\url{http://www.datasciencetoolkit.org/about}) or (2) Google
#' Maps. Note that when using Google you are agreeing to the Google
#' Maps API Terms of Service at
#' \url{https://developers.google.com/maps/terms}.
#'
#' Note that the Google Maps API limits to 2500 queries a day. Use
#' \code{geocodeQueryCheck} to determine how many queries remain.
#'
#' @param location a character vector of street addresses or place
#'   names (e.g. "1600 pennsylvania avenue, washington dc" or
#'   "Baylor University")
#' @param output amount of output, "latlon", "latlona", "more", or
#'   "all"
#' @param source "dsk" for Data Science Toolkit or "google" for
#'   Google
#' @param messaging turn messaging on/off
#' @param force force online query, even if previously downloaded
#' @param override_limit override the current query count
#'   (.GoogleGeocodeQueryCount)
#' @param nameType in some cases, Google returns both a long name
#'   and a short name. this parameter allows the user to specify
#'   which to grab.
#' @param inject character string to add to the url
#' @param ... ...
#' @return If \code{output} is "latlon", "latlona", or "more", a
#'   data frame. If all, a list.
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{mutate_geocode}},
#'   \url{http://code.google.com/apis/maps/documentation/geocoding/},
#'   \url{https://developers.google.com/maps/documentation/geocoding/usage-limits}
#' @export
#' @examples
#'
#' \dontrun{ # Server response can be slow; this cuts down check time.
#'
#' ##### basic usage
#' ########################################
#'
#' geocode("houston texas")
#' geocode("1600 pennsylvania avenue, washington dc")
#' geocode("the white house")
#' geocode(c("the white house", "washington dc"))
#' # see also mutate_geocode()
#'
#'
#' ##### types of output
#' ########################################
#'
#' # types of output
#' geocode("houston texas", output = "latlona")
#' geocode("houston texas", output = "more")
#' geocode("Baylor University", output = "more")
#' str(geocode("Baylor University", output = "all"))
#'
#'
#' ##### interfacing with the google geocoding api
#' ########################################
#'
#' register_google(key = "your code here")
#' geocode("houston texas")
#'
#'
#' # see how many requests we have left with google
#' geocodeQueryCheck()
#' geocode("one bear place, waco, texas")
#' geocode("houston texas", force = TRUE)
#'
#'
#'
#' ##### known issues
#' ########################################
#' # sources often have a hard time reliably geocoding colloquial place names
#' geocode("city hall houston")
#' geocode("city hall houston texas")
#' geocode("rice university")
#' geocode("rice university houston texas")
#'
#'
#' }
#'
geocode <- function(location, output = c("latlon", "latlona", "more", "all"),
    source = c("google", "dsk"), messaging = FALSE,
    force = ifelse(source == "dsk", FALSE, TRUE),
    override_limit = FALSE, nameType = c("long", "short"),
    inject = "", ...
){

  # basic parameter check
  stopifnot(is.character(location))
  stopifnot(is.logical(messaging))
  output   <- match.arg(output)
  nameType <- match.arg(nameType)
  source   <- match.arg(source)



  # vectorize for many locations
  if(length(location) > 1){
    # set limit
    if(goog_account() == "standard"){
      limit <- "2500"
    } else if(goog_account() == "premium"){
      limit <- "100000"
    }

    # message/stop as neeeded
    s <- sprintf("google restricts requests to %s requests a day for non-premium use.", limit)
    if(length(location) > as.numeric(limit)) stop(s, call. = FALSE)
    if(length(location) > 200 && messaging) message(paste("Reminder", s, sep = " : "))

    # geocode ply and out
    if(output == "latlon" || output == "latlona" || output == "more"){
      return(ldply(as.list(location), geocode, output = output, source = source, messaging = messaging, inject = inject))
    } else { # output = all
      return(llply(as.list(location), geocode, output = output, source = source, messaging = messaging, inject = inject))
    }
  }



  # return NA for location == ""
  if(location == "") return(failedGeocodeReturn(output))



  # start constructing the url
  posturl <- URLencode(location, reserved = TRUE)

  if(source == "google"){

    # add google account stuff
    if (has_client() && has_signature()) {
      client <- goog_client()
      signature <- goog_signature()
      posturl <- paste(posturl, fmteq(client), fmteq(signature), sep = "&")
    } else if (has_key()) {
      key <- goog_key()
      posturl <- paste(posturl, fmteq(key), sep = "&")
    }

    # add to url
    url_string <- paste0("https://maps.googleapis.com/maps/api/geocode/json?address=", posturl)

  } else if(source == "dsk"){
    url_string <- paste0("http://www.datasciencetoolkit.org/maps/api/geocode/json?address=", posturl)
  }

  # inject any remaining stuff
  url_string <- paste0(url_string, inject)

  # encode
  url_string <- URLencode( enc2utf8(url_string) )
  url_hash   <- digest::digest(url_string)



  # lookup info if on file
  if(isGeocodedInformationOnFile(url_hash) && force == FALSE){

  	if(messaging) message("Using stored information.")
    gc <- get(".GeocodedInformation", envir = .GlobalEnv)[[url_hash]]

  } else {

    if(messaging) message(paste("contacting ", url_string, "...", sep = ""), appendLF = F)

    # if using google, check/update google query limit
    if(source == "google"){
      check <- checkGeocodeQueryLimit(
        url_hash, elems = 1, override = override_limit,
        messaging = messaging
      )
      if(check == "stop") return(failedGeocodeReturn(output))
    }

    # message user
    message("Source : ", url_string)

    # geocode
    connect <- url(url_string)
    lines <- try(readLines(connect, warn = FALSE), silent = TRUE)
    close(connect)

    if(class(lines) == "try-error"){
      warning(
        "  geocoding failed for \"", location, "\".\n",
        "  if accompanied by 500 Internal Server Error with using dsk, try google."
      )
      return(failedGeocodeReturn(output))
    }

    gc <- fromJSON(paste(lines, collapse = ""))
    if(messaging) message(" done.")


    # temporarily save it
    storeGeocodedInformation(url_hash, gc)

  }



  # return if you want full output
  if(output == "all") return(gc)



  # did geocode fail? - print(gc$status)
  if(gc$status != "OK"){
    warning(paste("geocode failed with status ", gc$status, ", location = \"",
      location, "\"", sep = ""), call. = FALSE)
    return(data.frame(lon = NA, lat = NA))
  }



  # more than one location found?
  if(length(gc$results) > 1 && messaging){
    message(
      "more than one location found for \"", location, "\", using address\n  \"",
      tolower(gc$results[[1]]$formatted_address), "\"\n"
    )
  }



  # format geocoded data
  NULLtoNA <- function(x){
    if(is.null(x)) return(NA)
    x
  }

  gcdf <- with(gc$results[[1]], {
  	data.frame(
      lon = NULLtoNA(geometry$location$lng),
      lat = NULLtoNA(geometry$location$lat),
      type = tolower(NULLtoNA(types[1])),
      loctype = tolower(NULLtoNA(geometry$location_type)),
      address = location, # dsk doesn't give the address
      north = NULLtoNA(geometry$viewport$northeast$lat),
      south = NULLtoNA(geometry$viewport$southwest$lat),
      east = NULLtoNA(geometry$viewport$northeast$lng),
      west = NULLtoNA(geometry$viewport$southwest$lng)
    )
  })



  # add address
  if(source == "google"){
    gcdf$address <- tolower(NULLtoNA(gc$results[[1]]$formatted_address))
  }

  if(output == "latlon") return(gcdf[,c("lon","lat")])
  if(output == "latlona") return(gcdf[,c("lon","lat","address")])



  # parse json when output == "more"
  nameToGrab   <- `if`(nameType == "long", "long_name", "short_name")
  outputVals  <- vapply(gc$results[[1]]$address_components, function(x) x[[nameToGrab]], character(1))
  outputNames <- vapply(gc$results[[1]]$address_components, function(x){
      if(length(x$types) == 0) return("query")
      x$types[1]
    },
    character(1)
  )
  gcdfMore <- as.data.frame(as.list(outputVals))
  names(gcdfMore) <- outputNames

  data.frame(gcdf, gcdfMore)
}












checkGeocodeQueryLimit <- function(url_hash, elems, override, messaging){

  .GoogleGeocodeQueryCount <- NULL; rm(.GoogleGeocodeQueryCount); # R CMD check trick

  if(exists(".GoogleGeocodeQueryCount", .GlobalEnv)){

    .GoogleGeocodeQueryCount <<- dplyr::filter(.GoogleGeocodeQueryCount, time >= Sys.time() - 24*60*60)

    # limit per 24 hours
    dayQueriesUsed <- sum(.GoogleGeocodeQueryCount$elements)
    if(dayQueriesUsed + elems > goog_day_limit()){
      message("query max exceeded, see ?geocode.  current total = ", dayQueriesUsed)
      if(!override) return("stop")
    }

    # limit per second
    secondQueriesUsed <- with(.GoogleGeocodeQueryCount, sum(elements[time >= Sys.time() - 1]))
    if(secondQueriesUsed + elems > goog_second_limit()){
      message(".", appendLF = FALSE)
      Sys.sleep(.2) # can do better
    }

    # append to .GoogleGeocodeQueryCount
    .GoogleGeocodeQueryCount <<- rbind(
      .GoogleGeocodeQueryCount,
      data.frame(
        time = Sys.time(),
        url = url_hash,
        elements = elems,
        stringsAsFactors = FALSE
      )
    )

  } else { # no geocodes on file

    .GoogleGeocodeQueryCount <<- data.frame(
      time = Sys.time(),
      url = url_hash,
      elements = elems,
      stringsAsFactors = FALSE
    )

  }

  invisible("go")
}











#' @export
#' @rdname geocode
geocodeQueryCheck <- function() {

  .GoogleGeocodeQueryCount <- NULL; rm(.GoogleGeocodeQueryCount);

  if(exists(".GoogleGeocodeQueryCount", .GlobalEnv)){

  	remaining <- goog_day_limit() - sum(
  	  dplyr::filter(.GoogleGeocodeQueryCount, time >= Sys.time() - 24*60*60)$elements
  	)
    message(remaining, " geocoding queries remaining.")

  } else {

  	remaining <- goog_day_limit()
    message(remaining, " geocoding queries remaining.")

  }

  invisible(remaining)
}





geoInfoDoesntExist <- function(){
  ".GeocodedInformation" %notin% ls(envir = .GlobalEnv, all.names =  TRUE)
}





storeGeocodedInformation <- function(url_hash, data){
  .GeocodedInformation <- NULL; rm(.GeocodedInformation)

  if(geoInfoDoesntExist()) .GeocodedInformation <<- list()

  db <- get(".GeocodedInformation", envir = .GlobalEnv)

  placesOnFile <- names(db)
  db <- c(db, list(data))
  names(db) <- c(placesOnFile, url_hash)

  .GeocodedInformation <<- db

  invisible()
}





retrieveGeocodedInformation <- function(url_hash){
  if(geoInfoDoesntExist()) return(NA)
  get(".GeocodedInformation", envir = .GlobalEnv)[[url_hash]]
}





isGeocodedInformationOnFile <- function(url_hash){
  if(geoInfoDoesntExist()) return(FALSE)
  if(url_hash %notin% names(get(".GeocodedInformation", envir = .GlobalEnv))) return(FALSE)
  TRUE
}





clearGeocodedInformation <- function(){
  # suppress in case it doesn't exist
  suppressWarnings(rm(".GeocodedInformation", envir = .GlobalEnv))
  invisible()
}





failedGeocodeReturn <- function(output){
  if(output == "latlon"){
    return(data.frame(lon = NA_real_, lat = NA_real_))
  } else if(output == "latlona"){
    return(c(lon = NA_real_, lat = NA_real_, address = NA_character_))
  } else if(output == "more") {
    return(c(
      lon = NA_real_, lat = NA_real_, type = NA_character_, loctype = NA_character_,
      address = NA_character_,
      north = NA_real_, south = NA_real_, east = NA_real_, west = NA_real_,
      locality = NA_character_, country = NA_character_
    ))
  } else {
    return(NA_real_)
  }
}





