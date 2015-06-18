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
#' @param location a character string specifying a location of interest (e.g.
#'   "Baylor University")
#' @param output amount of output, "latlon", "latlona", "more", or "all"
#' @param source "dsk" for Data Science Toolkit or "google" for Google
#' @param messaging turn messaging on/off
#' @param sensor whether or not the geocoding request comes from a device with a
#'   location sensor
#' @param override_limit override the current query count
#'   (.GoogleGeocodeQueryCount)
#' @param client client ID for business users, see
#'   \url{https://developers.google.com/maps/documentation/business/webservices/auth}
#' @param signature signature for business users, see
#'   \url{https://developers.google.com/maps/documentation/business/webservices/auth}
#' @param nameType in some cases, Google returns both a long name and a short
#'   name. this parameter allows the user to specify which to grab.
#' @param data a data frame
#' @return If \code{output} is "latlon", "latlona", or "more", a data frame. If
#'   all, a list.
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \url{http://code.google.com/apis/maps/documentation/geocoding/}
#' @export
#' @examples
#'
#' # types of input
#' geocode("houston texas")
#' geocode("baylor university", source = "google") # see known issues below
#' geocode("1600 pennsylvania avenue, washington dc", source = "google")
#' geocode("the white house", source = "google")
#' geocode(c("baylor university", "salvation army waco"), source = "google")
#'
#'
#' # types of output
#' geocode("houston texas", output = "latlona")
#' geocode("houston texas", output = "more", source = "google")
#' geocode("Baylor University", output = "more", source = "google")
#' str(geocode("Baylor University", output = "all"), source = "google")
#'
#'
#' # see how many requests we have left with google
#' geocodeQueryCheck()
#'
#' df <- data.frame(
#'   address = c(
#'     "1600 pennsylvania avenue, washington dc",
#'     "", # missing address
#'     "1100 congress avenue, austin, tx 78701"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#' geocode(address, data = df)
#'
#'
#' \dontrun{
#'
#' # known issues :
#' # (1) source = "dsk" can't reliably geocode colloquial locations
#' geocode("city hall houston")
#' geocode("rice university")
#'
#'
#' }
#'
geocode <- function(location, output = c("latlon", "latlona", "more", "all"),
    source = c("dsk", "google"),
    messaging = FALSE, sensor = FALSE, override_limit = FALSE,
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
    .Deprecated(msg = "this use of geocode is deprecated, use mutate_geocode instead.")

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


  # lookup info if on file
  if(isGeocodedInformationOnFile(location)){

  	if(messaging) message("Using stored information.")
    gc <- get(".GeocodedInformation", envir = .GlobalEnv)[[location]]

  } else {

    # format url
    sensor4url <- paste("sensor=", tolower(as.character(sensor)), sep = "")
    client4url <- paste("client=", client, sep = "")
    signature4url <- paste("signature=", signature, sep = "")
    loc <- location
    location <- gsub(" ", "+", location)
    posturl <- paste(location, sensor4url, sep = "&")
    if(userType == "business"){
    	  posturl <- paste(posturl, client4url, signature4url, sep = "&")
    	}
    if(source == "google"){
      url_string <- paste("http://maps.googleapis.com/maps/api/geocode/json?address=", posturl, sep = "")
    } else if(source == "dsk"){
      url_string <- paste("http://www.datasciencetoolkit.org/maps/api/geocode/json?address=", posturl, sep = "")
    }
    url_string <- URLencode(url_string)
    if(messaging) message(paste("contacting ", url_string, "...", sep = ""), appendLF = F)

    # if using google, check/update google query limit
    if(source == "google"){

      check <- checkGeocodeQueryLimit(
        url_string, elems = 1, override = override_limit, messaging = messaging, userType = userType
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
    storeGeocodedInformation(loc, gc)

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
      "more than one location found for \"", loc, "\", using address\n  \"",
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

  # add address if
  if(source == "google"){
    gcdf$address <- tolower(NULLtoNA(gc$results[[1]]$formatted_address))
  }

  if(output == "latlon") return(gcdf[,c("lon","lat")])
  if(output == "latlona") return(gcdf[,c("lon","lat","address")])


  # parse json when output == "more"
  ndxToGrab   <- `if`(nameType == "long", 1, 2)
  outputVals  <- vapply(gc$results[[1]]$address_components, function(x) x[[ndxToGrab]], character(1))
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












checkGeocodeQueryLimit <- function(url_string, elems, override, messaging, userType){

  .GoogleGeocodeQueryCount <- NULL; rm(.GoogleGeocodeQueryCount); # R CMD check trick

  stopifnot(userType %in% c("free", "business"))
  limit <- c("free" = 2500, "business" = 1E5)[userType]

  if(exists(".GoogleGeocodeQueryCount", .GlobalEnv)){

    .GoogleGeocodeQueryCount <<-
      subset(.GoogleGeocodeQueryCount, time >= Sys.time() - 24*60*60)

    # limit per 24 hours
    if(sum(.GoogleGeocodeQueryCount$elements) + elems > limit){
      message("query max exceeded, see ?geocode.  current total = ",
        sum(.GoogleGeocodeQueryCount$elements))
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
    .GoogleGeocodeQueryCount <<- rbind(.GoogleGeocodeQueryCount,
      data.frame(
        time = Sys.time(),
        url = url_string,
        elements = elems,
        stringsAsFactors = FALSE
      )
    )


  } else {

    .GoogleGeocodeQueryCount <<-
      data.frame(time = Sys.time(),  url = url_string,
        elements = elems, stringsAsFactors = FALSE)

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













storeGeocodedInformation <- function(location, data){
  .GeocodedInformation <- NULL; rm(.GeocodedInformation)

  if(!(".GeocodedInformation" %in% ls(envir = .GlobalEnv, all.names =  TRUE))){
    .GeocodedInformation <<- list()
  }

  db <- get(".GeocodedInformation", envir = .GlobalEnv)

  placesOnFile <- names(db)
  db <- c(db, list(data))
  names(db) <- c(placesOnFile, location)

  .GeocodedInformation <<- db

  invisible()

}








retrieveGeocodedInformation <- function(location){

  if(!(".GeocodedInformation" %in% ls(envir = .GlobalEnv, all.names =  TRUE))) return(NA)

  get(".GeocodedInformation", envir = .GlobalEnv)[[location]]

}









isGeocodedInformationOnFile <- function(location){

  if(!(".GeocodedInformation" %in% ls(envir = .GlobalEnv, all.names =  TRUE))) return(FALSE)

  if(!(location %in%
    names(get(".GeocodedInformation", envir = .GlobalEnv))
  )) return(FALSE)

  TRUE

}










clearGeocodedInformation <- function(){

  if(!(".GeocodedInformation" %in% ls(envir = .GlobalEnv, all.names =  TRUE))) return(invisible())

  rm(".GeocodedInformation", envir = .GlobalEnv)

  invisible()

}













failedGeocodeReturn <- function(output){

  if(output == "latlon"){
    return(data.frame(lon = NA, lat = NA))
  } else if(output == "latlona"){
    return(c(lon = NA, lat = NA, address = NA))
  } else if(output == "more") {
    return(c(lon = NA, lat = NA, type = NA, loctype = NA,
             address = NA, north = NA, south = NA, east = NA, west = NA, locality = NA,
             country = NA)
    )
  } else {
    return(NA)
  }

}











