#' Geocode
#'
#' Geocodes (finds latitude and longitude of) a location using the Google
#' Geocoding API. Note: To use Google's Geocoding API, you must first enable the
#' API in the Google Cloud Platform Console. See \code{?register_google}.
#'
#' @param location a character vector of street addresses or place names (e.g.
#'   "1600 pennsylvania avenue, washington dc" or "Baylor University")
#' @param output amount of output, "latlon", "latlona", "more", or "all"
#' @param source "google" for Google (note: "dsk" is defunct)
#' @param force force online query, even if cached (previously downloaded)
#' @param urlonly return only the url?
#' @param override_limit override the current query rate
#' @param nameType in some cases, Google returns both a long name and a short
#'   name. this parameter allows the user to specify which to grab.
#' @param ext top level domain (e.g. "com", "co.nz"); helpful for non-US users
#' @param inject character string to add to the url or named character vector of
#'   key-value pairs to be injected (e.g. c("a" = "b") get converted to "a=b"
#'   and appended to the query)
#' @param data a data frame or equivalent
#' @param ... ...
#' @return If \code{output} is "latlon", "latlona", or "more", a tibble (classed
#'   data frame). If "all", a list.
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \url{http://code.google.com/apis/maps/documentation/geocoding/},
#'   \url{https://developers.google.com/maps/documentation/javascript/geocoding},
#'   \url{https://developers.google.com/maps/documentation/geocoding/usage-limits}
#' @name geocode
#' @examples
#'
#' \dontrun{ requires Google API key, see ?register_google
#'
#' ## basic usage
#' ########################################
#'
#' # geocoding is most commonly used for addresses
#' geocode("1600 Amphitheatre Parkway, Mountain View, CA")
#' geocode("1600 Amphitheatre Parkway, Mountain View, CA", urlonly = TRUE)
#'
#' # google can also geocode colloquial names of places
#' geocode("the white house")
#'
#' # geocode can also accept character vectors of places
#' geocode(c("the white house", "washington dc"))
#'
#'
#'
#' ## types of output
#' ########################################
#'
#' geocode("waco texas")
#' geocode("waco texas", output = "latlona")
#' geocode("waco texas", output = "more")
#' str(geocode("waco texas", output = "all"))
#'
#' geocode(c("waco, texas", "houston, texas"))
#' geocode(c("waco, texas", "houston, texas"), output = "latlona")
#' geocode(c("waco, texas", "houston, texas"), output = "all") %>% str(4)
#'
#'
#'
#' ## mutate_geocode
#' ########################################
#'
#' # mutate_geocode is used to add location columns to an existing dataset
#' # that has location information
#'
#' df <- data.frame(
#'   address = c("1600 Pennsylvania Avenue, Washington DC", "", "houston texas"),
#'   stringsAsFactors = FALSE
#' )
#'
#' mutate_geocode(df, address)
#' df %>% mutate_geocode(address)
#'
#'
#' ## known issues
#' ########################################
#'
#' # in some cases geocode finds several locations
#' geocode("waco city hall")
#'
#'
#' }
#'
#'











#' @rdname geocode
#' @export
geocode <- function (
  location,
  output = c("latlon", "latlona", "more", "all"),
  source = c("google", "dsk"),
  force = ifelse(source == "dsk", FALSE, TRUE),
  urlonly = FALSE,
  override_limit = FALSE,
  nameType = c("long", "short"),
  ext = "com",
  inject = "",
  ...
) {

  # basic parameter check
  stopifnot(is.character(location))
  output <- match.arg(output)
  nameType <- match.arg(nameType)
  source   <- match.arg(source)


  # source checking
  if (source == "google" && !has_google_key() && !urlonly) stop("Google now requires an API key.", "\n       See ?register_google for details.", call. = FALSE)
  if (source == "dsk") stop("datasciencetoolkit.org terminated its map service, sorry!")


  # vectorize for many locations
  if (length(location) > 1) {

    out <- location %>% map(~ geocode(.x, "output" = output, "source" = source, "messaging" = messaging, "inject" = inject))

    if (output == "all") return(out)

    out <- out %>% map(~ as_tibble(as.list(.x))) %>% bind_rows()
    return(out)

  }


  # return NA for location == ""
  if (location == "") return(return_failed_geocode(output))


  # set url base (protocol + fqdn + path + "?")
  url_base <- switch(source,
    "google" = glue("https://maps.googleapis.{ext}/maps/api/geocode/json?"),
       "dsk" = "http://www.datasciencetoolkit.org/maps/api/geocode/json?"
  )


  # initialize the url query
  url_query <- location %>% str_trim() %>% str_replace_all(" +", "+") %>% URLencode(reserved = FALSE) %>% c("address" = .)
  #                                        address
  # "1600+Amphitheatre+Parkway,+Mountain+View,+CA"


  # add google account stuff to query, if applicable
  if (source == "google") {
    url_query <- c(url_query, "client" = google_client(), "signature" = google_signature(), "key" = google_key())
    url_query <- url_query[!is.na(url_query)]
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


  # return early if user only wants url
  if(urlonly) if(showing_key()) return(url) else return(scrub_key(url))


  # hash for caching
  url_hash <- digest::digest(url)


  # lookup info if on file
  if (location_is_cached(url_hash) && force == FALSE) {

    gc <- geocode_cache()[[url_hash]]

  } else {

    # if using google, throttle/update google query limit
    if (source == "google") throttle_google_geocode_query_rate(url_hash, queries_sought = 1L, override = override_limit)

    # message url
    if (showing_key()) message("Source : ", url) else message("Source : ", scrub_key(url))

    # query server
    response <- httr::GET(url)

    # deal with bad responses
    if (response$status_code != 200L) {
      warning(
        tryCatch(stop_for_status(response),
          "http_400" = function(c) "HTTP 400 Bad Request",
          "http_402" = function(c) "HTTP 402 Payment Required - May indicate over Google query limit",
          "http_403" = function(c) "HTTP 403 Forbidden - Server refuses, is the API enabled?",
          "http_404" = function(c) "HTTP 404 Not Found - Server reports page not found",
          "http_414" = function(c) "HTTP 414 URI Too Long - URL query too long",
          "http_500" = function(c) "HTTP 500 Internal Server Error - If dsk, try Google",
          "http_503" = function(c) "HTTP 503 Service Unavailable - Server bogged down, try later"
        )
      )
      return(return_failed_geocode(output))
    }

    # grab content
    gc <- httr::content(response)

    # cache it
    cache_geocoded_info(url_hash, gc)

  }


  # did geocode fail?
  if (gc$status != "OK") {
    warning(
      glue("Geocoding \"{str_trunc(location, 20)}\" failed with error:"),
      "\n", gc$error_message, "\n",
      call. = FALSE, immediate. = TRUE, noBreaks. = FALSE
    )
    return(tibble("lon" = NA_real_, "lat" = NA_real_))
  }


  # return if you want full output
  if (output == "all") return(gc)



  # more than one location found?
  if (length(gc$results) > 1L) {
    message( glue("\"{stringr::str_trunc(location, 20)}\" not uniquely geocoded, using \"{tolower(gc$results[[1]]$formatted_address)}\"") )
  }



  # format geocoded data
  NULLtoNA <- function (x) {
    if (is.null(x)) return(NA) else x
  }

  gcdf <- with(gc$results[[1]], {
  	tibble(
      "lon" = NULLtoNA(geometry$location$lng),
      "lat" = NULLtoNA(geometry$location$lat),
      "type" = tolower(NULLtoNA(types[1])),
      "loctype" = tolower(NULLtoNA(geometry$location_type)),
      "address" = location, # dsk doesn't give the address
      "north" = NULLtoNA(geometry$viewport$northeast$lat),
      "south" = NULLtoNA(geometry$viewport$southwest$lat),
      "east" = NULLtoNA(geometry$viewport$northeast$lng),
      "west" = NULLtoNA(geometry$viewport$southwest$lng)
    )
  })



  # add address
  if (source == "google") gcdf$address <- tolower(NULLtoNA(gc$results[[1]]$formatted_address))


  if (output == "latlon") return(gcdf[,c("lon","lat")])
  if (output == "latlona") return(gcdf[,c("lon","lat","address")])
  if (output == "more") return(gcdf)


  # parse json when output == "more"
  name_to_grab  <- if(nameType == "long") "long_name" else "short_name"
  output_values <- vapply(gc$results[[1]]$address_components, function (x) x[[name_to_grab]], character(1))
  output_names <- vapply(gc$results[[1]]$address_components, function (x) {
      if (length(x$types) == 0) return("query")
      unlist(x$types)[1]
    },
    character(1)
  )
  gcdf_more <- as_tibble(as.list(output_values))
  names(gcdf_more) <- output_names

  tibble(gcdf, gcdf_more)
}






















#' @rdname geocode
#' @export
mutate_geocode <- function (data, location, ...){
  locs <- data[[deparse(substitute(location))]]
  gcdf <- geocode(locs, ...)
  dplyr::bind_cols(data, gcdf)
}












throttle_google_geocode_query_rate <- function (url_hash, queries_sought, override) {

  if (exists(".google_geocode_query_times", ggmap_environment)) {

    .google_geocode_query_times <- get(".google_geocode_query_times", envir = ggmap_environment)

    queries_used_in_last_second <- with(.google_geocode_query_times, sum(queries[time >= Sys.time() - 1L]))

    if (!override && (queries_used_in_last_second + queries_sought > google_second_limit())) Sys.sleep(.2) # can do better

    assign(
      ".google_geocode_query_times",
      bind_rows(.google_geocode_query_times, tibble("time" = Sys.time(), "url" = url_hash, "queries" = queries_sought)),
      envir = ggmap_environment
    )

  } else {

    assign(".google_geocode_query_times", tibble("time" = Sys.time(), "url" = url_hash, "queries" = queries_sought), envir = ggmap_environment)

  }

  invisible()

}










#' @export
#' @rdname geocode
geocodeQueryCheck <- function () {

  .Deprecated(msg = "As of mid-2018, Google no longer has daily query limits.")
  queries <- NA; rm(queries)

  if (exists(".google_geocode_query_times", ggmap_environment)) {

    .google_geocode_query_times <- get(".google_geocode_query_times", ggmap_environment)

    google_geocode_queries_in_last_24hrs <-
      .google_geocode_query_times %>%
        dplyr::filter(time >= Sys.time() - 24L*60L*60L) %>%
        dplyr::select(queries) %>%
        sum()

  	remaining <- google_day_limit() - google_geocode_queries_in_last_24hrs
    message(remaining, " Google geocoding queries remaining.")

  } else {

  	remaining <- google_day_limit()
    message(remaining, " Google geocoding queries remaining.")

  }

  invisible(remaining)
}






geocode_cache <- function () get(".geocode_cache", envir = ggmap_environment)







cache_geocoded_info <- function (url_hash, data) {

  if (!exists(".geocode_cache", envir = ggmap_environment)) assign(".geocode_cache", list(), ggmap_environment)

  assign(
    ".geocode_cache",
    c(geocode_cache(), structure(list(data), names = url_hash)),
    envir = ggmap_environment
  )

  invisible()

}








location_is_cached <- function (url_hash) {
  if (!exists(".geocode_cache", envir = ggmap_environment)) return(FALSE)
  if (url_hash %notin% names(geocode_cache())) return(FALSE)
  TRUE
}










return_failed_geocode <- function (output) {
  if (output == "latlon") {
    return(tibble("lon" = NA_real_, "lat" = NA_real_))
  } else if (output == "latlona") {
    return(tibble("lon" = NA_real_, "lat" = NA_real_, "address" = NA_character_))
  } else if (output == "more") {
    return(tibble(
      "lon" = NA_real_, "lat" = NA_real_, "type" = NA_character_, "address" = NA_character_,
      "north" = NA_real_, "south" = NA_real_, "east" = NA_real_, "west" = NA_real_
    ))
  } else {
    return(NA)
  }
}

















