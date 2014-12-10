#' Geocode
#'
#' Geocodes a location (find latitude and longitude) using Google Maps.
#' Note that in most cases by using this function you are agreeing to the
#' Google Maps API Terms of Service at
#' \url{https://developers.google.com/maps/terms}.
#'
#' Note that the google maps api limits to 2500 queries a day. Use
#' \code{geocodeQueryCheck} to determine how many queries remain.
#'
#' @param location a character string specifying a location of interest
#'    (e.g. "Baylor University")
#' @param output amount of output
#' @param messaging turn messaging on/off
#' @param sensor whether or not the geocoding request comes from a device
#'   with a location sensor
#' @param override_limit override the current query count (.GoogleGeocodeQueryCount)
#' @param client client ID for business users, see
#'  \url{https://developers.google.com/maps/documentation/business/webservices/auth}
#' @param signature signature for business users, see
#'   \url{https://developers.google.com/maps/documentation/business/webservices/auth}
#' @param data a data frame
#' @return If \code{output} is "latlon", "latlona", or "more", a data frame.
#'   If all, a list.
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \url{http://code.google.com/apis/maps/documentation/geocoding/}
#' @export
#' @examples
#' # Types of input
#' geocode('Baylor University')
#' geocode('1600 Pennsylvania Avenue, Washington DC')
#' geocode('the white house')
#' geocode(c('baylor university', 'salvation army waco'))
#'
#' # Types of output
#' geocode('Baylor University', output = "latlona")
#' geocode('Baylor University', output = "more")
#' str(geocode('Baylor University', output = "all"))
#'
#' # See how many requests we have left
#' geocodeQueryCheck()
geocode <- function(location, output = c('latlon','latlona','more','all'),
                    messaging = FALSE, sensor = FALSE, override_limit = FALSE,
                    client = "", signature = "", data)
  {

  # check parameters
  if(missing(data)) stopifnot(is.character(location))
  output <- match.arg(output)
  stopifnot(is.logical(messaging))
  if(client != "" && signature != ""){
  	if(substr(client, 1, 4) != 'gme-') client <- paste("gme-", client, sep = "")
  	userType <- "business"
  } else if(client == "" && signature != ""){
    stop("if signature argument is specified, client must be as well.", call. = FALSE)
  } else if(client != "" && signature == ""){
    stop("if client argument is specified, signature must be as well.", call. = FALSE)
  } else {
    userType <- "free"
  }


  if(!missing(data)){
    argList <- as.list(match.call()[-1])
    argNames <- names(argList)
    if(output == "all"){
      message("output = \"all\" is not allowed with data; changing to \"more\".")
      output <- "more"
    }

    locs <- eval(substitute(location), data)
    geocodedLocs <- geocode(locs, output = output, messaging = messaging,
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

  # vectorize for many locations (divide and conquer)
  if(length(location) > 1){
  	if(userType == "free"){
      s <- 'google restricts requests to 2500 requests a day for non-business use.'
      if(length(location) > 2500) stop(s, call. = F)
      if(length(location) > 200 && messaging) message(paste('Reminder', s, sep = ' : '))
      if(output == 'latlon' || output == 'latlona' ||output == 'more'){
        return(ldply(as.list(location), geocode, output = output, messaging = messaging))
      } else { # output = all
        return(llply(as.list(location), geocode, output = output, messaging = messaging))
      }
    } else { # userType == "business"
      s <- 'google restricts requests to 100000 requests a day for business use.'
      if(length(location) > 100000) stop(s, call. = F)
      if(length(location) > 200 && messaging) message(paste('Reminder', s, sep = ' : '))
      if(output == 'latlon' || output == 'latlona' ||output == 'more'){
        return(ldply(as.list(location), geocode, output = output, messaging = messaging))
      } else { # output = all
        return(llply(as.list(location), geocode, output = output, messaging = messaging))
      }
    }
  }

  if(isGeocodedInformationOnFile(location)){

  	if(messaging) message("Using stored information.")
    gc <- get(".GeocodedInformation", envir = .GlobalEnv)[[location]]

  } else {

    # format url
    sensor4url <- paste('sensor=', tolower(as.character(sensor)), sep = '')
    client4url <- paste('client=', client, sep = '')
    signature4url <- paste('signature=', signature, sep = '')
    loc <- location
    location <- gsub(' ', '+', location)
    posturl <- paste(location, sensor4url, sep = '&')
    if(userType == "business"){
    	  posturl <- paste(posturl, client4url, signature4url, sep = '&')
    	}
    url_string <- paste('http://maps.googleapis.com/maps/api/geocode/json?address=', posturl, sep = "")
    url_string <- URLencode(url_string)
    if(messaging) message(paste('contacting ', url_string, '...', sep = ''), appendLF = F)

    # check/update google query limit
    check <- checkGeocodeQueryLimit(url_string, elems = 1,
      override = override_limit, messaging = messaging, userType = userType)
    if(check == "stop"){
      if(output == "latlon"){
        return(c(lon = NA, lat = NA))
      } else if(output == "latlona"){
        return(c(lon = NA, lat = NA, address = NA))
      } else if(output == "latlona") {
        return(c(lon = NA, lat = NA, type = NA, loctype = NA,
          address = NA, north = NA, south = NA, east = NA, west = NA, postal_code = NA,
          country = NA, street = NA, streetNo = NA, point_of_interest = NA, query = loc)
        )
      } else {
        return(NA)
      }
    }

    # message user
    message(paste0('Information from URL : ', url_string))

    # geocode
    connect <- url(url_string)
    gc <- fromJSON(paste(readLines(connect), collapse = ''))
    if(messaging) message(' done.')
    close(connect)

    # temporarily save it
    storeGeocodedInformation(loc, gc)

  }

  # return if you want full output
  if(output == 'all') return(gc)

  # did geocode fail? - print(gc$status)
  if(gc$status != 'OK'){
    warning(paste('geocode failed with status ', gc$status, ', location = "',
      location, '"', sep = ''), call. = FALSE)
    return(data.frame(lon = NA, lat = NA))
  }

  # more than one location found?
  if(length(gc$results) > 1 && messaging){
    message(paste(
      'more than one location found for "', loc, '", using address\n  "',
      tolower(gc$results[[1]]$formatted_address), '"\n', sep = ''))
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
      address = tolower(NULLtoNA(formatted_address)),
      north = NULLtoNA(geometry$viewport$northeast$lat),
      south = NULLtoNA(geometry$viewport$southwest$lat),
      east = NULLtoNA(geometry$viewport$northeast$lng),
      west = NULLtoNA(geometry$viewport$southwest$lng)
    )
  })

  if(output == 'latlon') return(gcdf[,c('lon','lat')])
  if(output == 'latlona') return(gcdf[,c('lon','lat','address')])

  # even more?
  attrdf <- ldply(gc$results[[1]]$address_components, function(l){
    as.data.frame(l, stringsAsFactors = FALSE)[1,]
  })
  attrdf <- attrdf[,c('types','long_name')]
  gcdf <- within(gcdf,{
    point_of_interest <-
      tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'point_of_interest']))
    streetNo <-
      as.numeric(NULLtoNA(attrdf$long_name[attrdf$types == 'street_number']))
    street <-
      tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'route']))
    locality <-
      tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'locality']))
    administrative_area_level_1 <-
      tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'administrative_area_level_1']))
    administrative_area_level_2 <-
      tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'administrative_area_level_2']))
    country <-
      tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'country']))
    postal_code <-
      tolower(NULLtoNA(attrdf$long_name[attrdf$types == 'postal_code']))
  })
  gcdf$query <- location

  # return output = 'more'
  return(gcdf)
}

checkGeocodeQueryLimit <- function(url_string, elems, override, messaging, userType){

  .GoogleGeocodeQueryCount <- NULL; rm(.GoogleGeocodeQueryCount); # R CMD check trick

  stopifnot(userType %in% c("free", "business"))
  limit <- c("free" = 2500, "business" = 1E5)[userType]

  if(exists('.GoogleGeocodeQueryCount', .GlobalEnv)){

    .GoogleGeocodeQueryCount <<-
      subset(.GoogleGeocodeQueryCount, time >= Sys.time() - 24*60*60)

    # limit per 24 hours
    if(sum(.GoogleGeocodeQueryCount$elements) + elems > limit){
      message('query max exceeded, see ?geocode.  current total = ',
        sum(.GoogleGeocodeQueryCount$elements))
      if(!override) return("stop")
    }

    # 10 per 1 second?
    if(with(.GoogleGeocodeQueryCount,
      sum(elements[time >= Sys.time() - 10]) + elems > 10
    )){
      message('.', appendLF = FALSE)
      Sys.sleep(1) # can do better
    }

    # append to .GoogleGeocodeQueryCount
    .GoogleGeocodeQueryCount <<- rbind(.GoogleGeocodeQueryCount,
      data.frame(time = Sys.time(),  url = url_string,
        elements = elems, stringsAsFactors = FALSE)
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

  .GoogleGeocodeQueryCount <- NULL; rm(.GoogleGeocodeQueryCount); # R CMD check trick

  stopifnot(userType %in% c("free", "business"))
  limit <- c("free" = 2500, "business" = 1E5)[userType]

  if(exists('.GoogleGeocodeQueryCount', .GlobalEnv)){

  	remaining <- limit - sum(
  	  subset(.GoogleGeocodeQueryCount, time >= Sys.time() - 24*60*60)$elements
  	  )
    message(remaining, ' geocoding queries remaining.')

  } else {

  	remaining <- limit
    message(remaining, ' geocoding queries remaining.')

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
