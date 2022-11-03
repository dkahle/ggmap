#' Reverse geocode
#'
#' Reverse geocodes (looks up the address of) a longitude/latitude location
#' using the Google Geocoding API. Note: To use Google's Geocoding API, you must
#' first enable the API in the Google Cloud Platform Console. See
#' [register_google()].
#'
#' @param location a location in longitude/latitude format
#' @param output "address" or "all"
#' @param force force online query, even if cached (previously downloaded)
#' @param urlonly return only the url?
#' @param override_limit override the current query rate
#' @param ext top level domain extension (e.g. "com", "co.nz")
#' @param inject character string to add to the url
#' @param ... ...
#' @return a character(1) address or a list (the parsed json output from Google)
#' @author David Kahle \email{david@@kahle.io}
#' @seealso \url{https://developers.google.com/maps/documentation/geocoding/}
#' @export
#' @examples
#'
#' \dontrun{ requires Google API key, see ?register_google
#'
#' ## basic usage
#' ########################################
#'
#' ( gc <- as.numeric(geocode("the white house")) )
#' revgeocode(gc)
#' str(revgeocode(gc, output = "all"), 3)
#'
#' }
#'
revgeocode <- function (
  location,
  output = c("address","all"),
  force = FALSE,
  urlonly = FALSE,
  override_limit = FALSE,
  ext = "com",
  inject = "",
  ...
) {


  # check parameters
  stopifnot(is.numeric(location) && length(location) == 2)
  output <- match.arg(output)
  stopifnot(is.logical(override_limit))

  if (!has_google_key() && !urlonly) {
    cli::cli_abort("Google now requires an API key; see {.fn ggmap::register_google}.")
  }


  # form url base
  url_base <- glue("https://maps.googleapis.{ext}/maps/api/geocode/json?")


  # form query
  url_query <- c("latlng" = glue("{location[2]},{location[1]}"))


  # add google account stuff to query, if applicable
  url_query <- c(url_query, "client" = google_client(), "signature" = google_signature(), "key" = google_key())
  url_query <- url_query[!is.na(url_query)]


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
  url <- str_replace_all(url, "#", "%23") # selectively url-encode


  # return early if user only wants url
  if(urlonly) if(showing_key()) return(url) else return(scrub_key(url))


  # hash for caching
  url_hash <- digest::digest(scrub_key(url))


  # lookup info if on file
  if (location_is_cached(url_hash) && force == FALSE) {

    gc <- geocode_cache()[[url_hash]]

  } else {

    throttle_google_geocode_query_rate(url_hash, queries_sought = 1L, override = override_limit)

    # message url
    if (showing_key()) source_url_msg(url) else source_url_msg(scrub_key(url))

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
          "http_500" = function(c) "HTTP 500 Internal Server Error",
          "http_503" = function(c) "HTTP 503 Service Unavailable - Server bogged down, try later"
        )
      )
      return(NA_character_)
    }

    # grab content
    gc <- httr::content(response)

    # cache it
    cache_geocoded_info(url_hash, gc)

  }


  # did geocode fail?
  if (gc$status != "OK") {
    warning(
      glue("Reverse geocoding failed with error:"),
      "\n", gc$error_message, "\n",
      call. = FALSE, immediate. = TRUE, noBreaks. = FALSE
    )
    return(NA_character_)
  }


  # return if you want full output
  if (output == "all") return(gc)


  # more than one location found?
  if (length(gc$results) > 1L) {
    cli::cli_warn("Multiple addresses found, the first will be returned:")
    gc$results %>%
      map_chr(~ .x$formatted_address) %>%
      unique() %>%
      str_c("  ", .) %>%
      walk(cli::cli_alert_warning)
  }


  # return
  gc$results[[1]]$formatted_address
}


























