#' Geocode
#'
#' Geocodes a location (find latitude and longitude) using either (1) the Data
#' Science Toolkit (\url{http://www.datasciencetoolkit.org/about}) or (2) Google
#' Maps. Note that when using Google you are agreeing to the Google Maps API
#' Terms of Service at \url{https://developers.google.com/maps/terms}.
#'
#' Note that the Google Maps API limits to 2500 queries a day. Use
#' \code{geocodeQueryCheck} to determine how many queries remain.
#'
#' @param location a character vector of street addresses or place names (e.g.
#'   "1600 pennsylvania avenue, washington dc" or "Baylor University")
#' @param output amount of output, "latlon", "latlona", "more", or "all"
#' @param source "google" for Google, "dsk" for Data Science Toolkit terminated
#'   its service
#' @param messaging turn messaging on/off
#' @param force force online query, even if previously downloaded
#' @param urlonly return only the url?
#' @param override_limit override the current query count
#'   (.GoogleGeocodeQueryCount)
#' @param nameType in some cases, Google returns both a long name and a short
#'   name. this parameter allows the user to specify which to grab.
#' @param ext top level domain (e.g. "com", "co.nz"); helpful for non-US users
#' @param inject character string to add to the url or named character vector of
#'   key-value pairs to be injected (e.g. c("a" = "b") get converted to "a=b"
#'   and appended to the query)
#' @param ... ...
#' @return If \code{output} is "latlon", "latlona", or "more", a data frame. If
#'   all, a list.
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{mutate_geocode}},
#'   \url{http://code.google.com/apis/maps/documentation/geocoding/},
#'   \url{https://developers.google.com/maps/documentation/geocoding/usage-limits}
#'
#'
#'
#'
#'
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
#' geocode("ninos", inject = "region=es", urlonly = TRUE)
#' geocode("ninos", inject = c("region" = "es"), urlonly = TRUE)
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
#' # sources sometime have a hard time reliably geocoding colloquial place names
#'
#' # datasciencetoolkit.org no longer serves map tiles
#' # geocode("houston texas", source = "dsk")
#'
#' }
#'
geocode <- function (location, output = c("latlon", "latlona", "more", "all"),
    source = c("google", "dsk"), messaging = FALSE,
    force = ifelse(source == "dsk", FALSE, TRUE), urlonly = FALSE,
    override_limit = FALSE, nameType = c("long", "short"),
    ext = "com", inject = "", ...
) {

  # basic parameter check
  stopifnot(is.character(location))
  stopifnot(is.logical(messaging))
  output   <- match.arg(output)
  nameType <- match.arg(nameType)
  source   <- match.arg(source)



  # source checking
  if (source == "google" && !has_google_key())
    stop("Google now requires a (free) API key, see ?register_google")

  if (source == "dsk")
    stop("datasciencetoolkit.org terminated its map service, sorry!")


  # vectorize for many locations
  if (length(location) > 1) {
    # # set limit
    # if (has_google_account() && google_account() == "standard") {
    #   limit <- "2500"
    # } else if (has_google_account() && google_account() == "premium") {
    #   limit <- "100000"
    # } else { # if ggmap's not loaded
    #   limit <- "2500"
    # }

    # message/stop as neeeded
    # s <- sprintf("google restricts requests to %s requests a day for non-premium use.", limit)
    # if (length(location) > as.numeric(limit)) stop(s, call. = FALSE)
    # if (length(location) > 200 && messaging) message(str_c("Reminder", s, sep = " : "))

    # geocode ply and out
    if (output == "latlon" || output == "latlona" || output == "more") {
      return(ldply(as.list(location), geocode, output = output, source = source, messaging = messaging, inject = inject))
    } else { # output = all
      return(llply(as.list(location), geocode, output = output, source = source, messaging = messaging, inject = inject))
    }
  }



  # return NA for location == ""
  if (location == "") return(failedGeocodeReturn(output))


  # set url base (protocol + fqdn + path + "?")
  url_base <- switch(source,
    "google" = glue("https://maps.googleapis.{ext}/maps/api/geocode/json?"),
       "dsk" = "http://www.datasciencetoolkit.org/maps/api/geocode/json?"
  )


  # initialize the url query
  url_query <- c("address" = URLencode(location, reserved = TRUE))


  # add google account stuff to query, if applicable
  if (source == "google") {
    if (has_google_client() && has_google_signature()) {
      url_query <- c(url_query, "client" = google_client(), "signature" = google_signature())
    } else if (has_google_key()) {
      url_query <- c(url_query, "key" = google_key())
    }
  }


  # form url
  url_query_inline <- str_c(names(url_query), url_query, sep = "=", collapse = "&")
  url <- str_c(url_base, url_query_inline)


  # inject any remaining stuff
  if (inject != "") {
    if (is.null(names(inject))) {
      url <- str_c(url, inject, sep = "&")
    } else {
      url <- str_c(url, str_c(names(inject), inject, sep = "=", collapse = "&"), sep = "&")
    }
  }

  # encode
  url <- URLencode( enc2utf8(url) )
  if (urlonly) return(url)
  url_hash <- digest::digest(url)



  # lookup info if on file
  if (isGeocodedInformationOnFile(url_hash) && force == FALSE) {

  	if (messaging) message("Using stored information.")
    gc <- get(".GeocodedInformation", envir = .GlobalEnv)[[url_hash]]

  } else {

    # if using google, check/update google query limit
    if (source == "google") {
      check <- checkGeocodeQueryLimit(url_hash, elems = 1, override = override_limit, messaging = messaging)
      if (check == "stop") return(failedGeocodeReturn(output))
    }

    # query server
    if (showing_key()) {
      message("Source : ", url)
    } else {
      message("Source : ", scrub_key(url))
    }
    response <- httr::GET(url)
    if (messaging) message(" done.")

    # deal with bad responses
    if (response$status_code != 200) {
      warning(
        tryCatch(stop_for_status(response),
          http_400 = function(c) "HTTP 400 Bad Request - Bad inject?",
          http_402 = function(c) "HTTP 402 Payment Required - May indicate over Google query limit",
          http_403 = function(c) "HTTP 403 Forbidden - Server refuses query",
          http_404 = function(c) "HTTP 404 Not Found - Server reports page not found",
          http_414 = function(c) "HTTP 414 URI Too Long - URL query too long",
          http_500 = function(c) "HTTP 500 Internal Server Error - If dsk, try Google",
          http_503 = function(c) "HTTP 503 Service Unavailable - Server bogged down, try later"
        )
      )
      return(failedGeocodeReturn(output))
    }

    # grab content
    gc <- httr::content(response)

    # temporarily save it
    storeGeocodedInformation(url_hash, gc)

  }



  # return if you want full output
  if (output == "all") return(gc)



  # did geocode fail? - print(gc$status)
  if (gc$status != "OK") {
    warning(str_c("geocode failed with status ", gc$status, ", location = \"",
      location, "\"", sep = ""), call. = FALSE)
    return(data.frame(lon = NA, lat = NA))
  }



  # more than one location found?
  if (length(gc$results) > 1 && messaging) {
    message(
      "more than one location found for \"", location, "\", using address\n  \"",
      tolower(gc$results[[1]]$formatted_address), "\"\n"
    )
  }



  # format geocoded data
  NULLtoNA <- function (x) {
    if (is.null(x)) return(NA)
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
  if (source == "google") {
    gcdf$address <- tolower(NULLtoNA(gc$results[[1]]$formatted_address))
  }

  if (output == "latlon") return(gcdf[,c("lon","lat")])
  if (output == "latlona") return(gcdf[,c("lon","lat","address")])


  # parse json when output == "more"
  name_to_grab   <- if(nameType == "long") "long_name" else "short_name"
  output_values  <- vapply(gc$results[[1]]$address_components, function (x) x[[name_to_grab]], character(1))
  output_names <- vapply(gc$results[[1]]$address_components, function (x) {
      if (length(x$types) == 0) return("query")
      unlist(x$types)[1]
    },
    character(1)
  )
  gcdf_more <- as.data.frame(as.list(output_values))
  names(gcdf_more) <- output_names

  data.frame(gcdf, gcdf_more)
}












checkGeocodeQueryLimit <- function (url_hash, elems, override, messaging) {

  .GoogleGeocodeQueryCount <- NULL; rm(.GoogleGeocodeQueryCount); # R CMD check trick

  if (exists(".GoogleGeocodeQueryCount", .GlobalEnv)) {

    .GoogleGeocodeQueryCount <<- dplyr::filter(.GoogleGeocodeQueryCount, time >= Sys.time() - 24*60*60)

    # limit per 24 hours
    dayQueriesUsed <- sum(.GoogleGeocodeQueryCount$elements)
    if (dayQueriesUsed + elems > google_day_limit()) {
      message("query max exceeded, see ?geocode.  current total = ", dayQueriesUsed)
      if (!override) return("stop")
    }

    # limit per second
    secondQueriesUsed <- with(.GoogleGeocodeQueryCount, sum(elements[time >= Sys.time() - 1]))
    if (secondQueriesUsed + elems > google_second_limit()) {
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
geocodeQueryCheck <- function () {

  .GoogleGeocodeQueryCount <- NULL; rm(.GoogleGeocodeQueryCount);

  if (exists(".GoogleGeocodeQueryCount", .GlobalEnv)) {

  	remaining <- google_day_limit() - sum(
  	  dplyr::filter(.GoogleGeocodeQueryCount, time >= Sys.time() - 24*60*60)$elements
  	)
    message(remaining, " geocoding queries remaining.")

  } else {

  	remaining <- google_day_limit()
    message(remaining, " geocoding queries remaining.")

  }

  invisible(remaining)
}





geoInfoDoesntExist <- function () {
  ".GeocodedInformation" %notin% ls(envir = .GlobalEnv, all.names =  TRUE)
}





storeGeocodedInformation <- function (url_hash, data) {
  .GeocodedInformation <- NULL; rm(.GeocodedInformation)

  if (geoInfoDoesntExist()) .GeocodedInformation <<- list()

  db <- get(".GeocodedInformation", envir = .GlobalEnv)

  placesOnFile <- names(db)
  db <- c(db, list(data))
  names(db) <- c(placesOnFile, url_hash)

  .GeocodedInformation <<- db

  invisible()
}





retrieveGeocodedInformation <- function (url_hash) {
  if (geoInfoDoesntExist()) return(NA)
  get(".GeocodedInformation", envir = .GlobalEnv)[[url_hash]]
}





isGeocodedInformationOnFile <- function (url_hash) {
  if (geoInfoDoesntExist()) return(FALSE)
  if (url_hash %notin% names(get(".GeocodedInformation", envir = .GlobalEnv))) return(FALSE)
  TRUE
}





clearGeocodedInformation <- function () {
  # suppress in case it doesn't exist
  suppressWarnings(rm(".GeocodedInformation", envir = .GlobalEnv))
  invisible()
}





failedGeocodeReturn <- function (output) {
  if (output == "latlon") {
    return(data.frame(lon = NA_real_, lat = NA_real_))
  } else if (output == "latlona") {
    return(c(lon = NA_real_, lat = NA_real_, address = NA_character_))
  } else if (output == "more") {
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





