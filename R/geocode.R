#' Geocode
#'
#' Geocodes a location (find latitude and longitude) using either (1) the Data
#' Science Toolkit (\url{http://www.datasciencetoolkit.org/about}) or (2) Google
#' Maps. Note that when using Google you are agreeing to the Google Maps API
#' Terms of Service at \url{https://developers.google.com/maps/terms}.
#'
#' Note that the Google Maps api limits to 2500 queries a day. Use
#' \code{geocodeQueryCheck} to determine how many queries remain.
#'
#' @param location a character vector of street addresses or place names (e.g.
#'   "1600 pennsylvania avenue, washington dc" or "Baylor University")
#' @param output amount of output, "latlon", "latlona", "more", or "all"
#' @param source "dsk" for Data Science Toolkit or "google" for Google
#' @param messaging turn messaging on/off
#' @param force force online query, even if previously downloaded
#' @param sensor whether or not the geocoding request comes from a device with a
#'   location sensor
#' @param override_limit override the current query count
#'   (.GoogleGeocodeQueryCount)
#' @param client client ID for business users, see
#'   \url{https://developers.google.com/maps/documentation/business/webservices/auth}
#'
#' @param signature signature for business users, see
#'   \url{https://developers.google.com/maps/documentation/business/webservices/auth}
#'
#' @param nameType in some cases, Google returns both a long name and a short
#'   name. this parameter allows the user to specify which to grab.
#' @param data deprecated in 2.5, use \code{\link{mutate_geocode}}
#' @return If \code{output} is "latlon", "latlona", or "more", a data frame. If
#'   all, a list.
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{mutate_geocode}},
#'   \url{http://code.google.com/apis/maps/documentation/geocoding/}
#' @export
#' @examples
#'
#' \dontrun{ # Server response can be slow; this cuts down check time.
#'
#' # types of input
#' geocode("houston texas")
#' geocode("baylor university") # see known issues below
#' geocode("1600 pennsylvania avenue, washington dc")
#' geocode("the white house")
#' geocode(c("baylor university", "salvation army waco"))
#'
#'
#'
#'
#' # types of output
#' geocode("houston texas", output = "latlona")
#' geocode("houston texas", output = "more")
#' geocode("Baylor University", output = "more")
#' str(geocode("Baylor University", output = "all"))
#'
#'
#' # see how many requests we have left with google
#' geocodeQueryCheck()
#' geocode("one bear place, waco, texas")
#' geocode("houston texas", force = TRUE)
#'
#'
#'
#' # known issues :
#' # (1) source = "dsk" can't reliably geocode colloquial place names
#' geocode("city hall houston")
#' geocode("rice university")
#'
#'
#' }
#'
geocode <- function(location, output = c("latlon", "latlona", "more", "all"),
    source = c("google", "dsk"), messaging = FALSE,
    force = ifelse(source == "dsk", FALSE, TRUE), sensor = FALSE,
    override_limit = FALSE,
    client = "", signature = "", nameType = c("long", "short"), data
){

  # basic parameter check
  if(missing(data)) stopifnot(is.character(location))
  stopifnot(is.logical(messaging))
  output   <- match.arg(output)
  nameType <- match.arg(nameType)
  source   <- match.arg(source)



  # deal with client and signature
  if(client != "" && signature != ""){
  	if(substr(client, 1, 4) != "gme-") client <- paste("gme-", client, sep = "")
  	userType <- "business"
  } else if(client == "" && signature != ""){
    stop("if signature argument is specified, client must be as well.", call. = FALSE)
  } else if(client != "" && signature == ""){
    stop("if client argument is specified, signature must be as well.", call. = FALSE)
  } else {
    userType <- "free"
  }



  # deal with data
  if(!missing(data)){
    warning("This use of geocode is deprecated, use mutate_geocode() instead.", call. = FALSE)

    argList <- as.list(match.call()[-1])
    argNames <- names(argList)
    if(output == "all"){
      message("output = \"all\" is not allowed with data; changing to \"more\".")
      output <- "more"
    }

    locs <- eval(substitute(location), data)
    geocodedLocs <- geocode(locs, output = output, source = source, messaging = messaging,
      override_limit = override_limit, sensor = sensor, client = client,
      signature = signature)
    dataSetName <- as.character(substitute(data))
    # this works, but apparently violates crans rules
    message(paste0("overwriting dataset ", dataSetName, "."))
    saveOverCode <- paste0(dataSetName, " <<- data.frame(data, geocodedLocs)")
    eval(parse(text = saveOverCode))
    #assign(dataSetName, data.frame(data, geocodedLocs), envir = .GlobalEnv)
    return(invisible())
  }



  # vectorize for many locations
  if(length(location) > 1){
    # set limit
    if(userType == "free"){
      limit <- "2500"
    } else if(userType == "business"){
      limit <- "100000"
    }

    # message/stop as neeeded
    s <- paste("google restricts requests to", limit, "requests a day for non-business use.")
    if(length(location) > as.numeric(limit)) stop(s, call. = F)
    if(length(location) > 200 && messaging) message(paste("Reminder", s, sep = " : "))

    # geocode ply and out
    if(output == "latlon" || output == "latlona" || output == "more"){
      return(ldply(as.list(location), geocode, output = output, source = source, messaging = messaging))
    } else { # output = all
      return(llply(as.list(location), geocode, output = output, source = source, messaging = messaging))
    }
  }



  # return NA for location == ""
  if(location == "") return(failedGeocodeReturn(output))



  # format the url
  sensor4url <- paste("sensor=", tolower(as.character(sensor)), sep = "")
  client4url <- paste("client=", client, sep = "")
  signature4url <- paste("signature=", signature, sep = "")
  location4url <- chartr(" ", "+", location)
  posturl <- paste(location, sensor4url, sep = "&")
  if(userType == "business") posturl <- paste(posturl, client4url, signature4url, sep = "&")

  if(source == "google"){
    url_string <- paste("http://maps.googleapis.com/maps/api/geocode/json?address=", posturl, sep = "")
  } else if(source == "dsk"){
    url_string <- paste("http://www.datasciencetoolkit.org/maps/api/geocode/json?address=", posturl, sep = "")
  }

  url_string <- URLencode(url_string)
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
        messaging = messaging, userType = userType
      )
      if(check == "stop") return(failedGeocodeReturn(output))
    }

    # message user
    message(paste0("Information from URL : ", url_string))

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
    message(paste(
      "more than one location found for \"", location, "\", using address\n  \"",
      tolower(gc$results[[1]]$formatted_address), "\"\n", sep = ""))
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












checkGeocodeQueryLimit <- function(url_hash, elems, override, messaging, userType){

  .GoogleGeocodeQueryCount <- NULL; rm(.GoogleGeocodeQueryCount); # R CMD check trick

  stopifnot(userType %in% c("free", "business"))
  limit <- c("free" = 2500, "business" = 1E5)[userType]

  if(exists(".GoogleGeocodeQueryCount", .GlobalEnv)){

    .GoogleGeocodeQueryCount <<- subset(.GoogleGeocodeQueryCount,
      time >= Sys.time() - 24*60*60
    )

    nQueriesUsed <- sum(.GoogleGeocodeQueryCount$elements)

    # limit per 24 hours
    if(nQueriesUsed + elems > limit){
      message("query max exceeded, see ?geocode.  current total = ", nQueriesUsed)
      if(!override) return("stop")
    }

    # 10 per 1 second?
    if(with(.GoogleGeocodeQueryCount,
      sum(elements[time >= Sys.time() - 10]) + elems > 10
    )){
      message(".", appendLF = FALSE)
      Sys.sleep(1) # can do better
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
#' @param userType User type, "free" or "business"
#' @rdname geocode
geocodeQueryCheck <- function(userType = "free"){

  .GoogleGeocodeQueryCount <- NULL; rm(.GoogleGeocodeQueryCount);

  stopifnot(userType %in% c("free", "business"))
  limit <- c("free" = 2500, "business" = 1E5)[userType]

  if(exists(".GoogleGeocodeQueryCount", .GlobalEnv)){

  	remaining <- limit - sum(
  	  subset(.GoogleGeocodeQueryCount, time >= Sys.time() - 24*60*60)$elements
  	)
    message(remaining, " geocoding queries remaining.")

  } else {

  	remaining <- limit
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





