#' Grab a trek from Google
#'
#' Sequence treks (latitude-longitude sequences following ordinary paths, e.g.
#' roads) between two locations using the Google Directions API. Note: To use
#' Google's Directions API, you must first enable the API in the Google Cloud
#' Platform Console. See \code{?register_google}.
#'
#' @param from name of origin addresses in a data frame
#' @param to name of destination addresses in a data frame
#' @param output amount of output ("simple" or "all")
#' @param mode driving, bicycling, walking, or transit
#' @param alternatives should more than one route be provided?
#' @param units "metric"
#' @param urlonly return only the url?
#' @param override_limit override the current query count
#' @param ext domain extension (e.g. "com", "co.nz")
#' @param inject character string to add to the url
#' @param ... ...
#' @return a tibble
#' @author David Kahle \email{david.kahle@@gmail.com} with the key decoding
#'   algorithm due to Stack Overflow user akhmed
#' @seealso \url{https://developers.google.com/maps/documentation/directions/},
#'   \url{http://stackoverflow.com/questions/30270011/ggmap-route-finding-doesnt-stay-on-roads},
#'    \code{\link{route}}, \code{\link{routeQueryCheck}}
#'   \code{\link{register_google}}
#' @export
#' @examples
#'
#' \dontrun{ requires Google API key, see ?register_google
#'
#' ## basic usage
#' ########################################
#'
#' from <- "houston, texas"
#' to <- "waco, texas"
#'
#' (route_df <- route(from, to, structure = "route"))
#' (trek_df  <-  trek(from, to, structure = "route"))
#'
#' qmap("college station, texas", zoom = 8) +
#'   geom_path(
#'     aes(x = lon, y = lat),  colour = "red",
#'     size = 1.5, alpha = .5,
#'     data = route_df, lineend = "round"
#'   ) +
#'   geom_path(
#'     aes(x = lon, y = lat),  colour = "blue",
#'     size = 1.5, alpha = .5,
#'     data = trek_df, lineend = "round"
#'   )
#'
#'
#'
#' from <- "rice university houston texas"
#' to <- "1001 Bissonnet St, Houston, TX 77005"
#' trek_df <- trek(from, to)
#' qmplot(lon, lat, data = trek_df, geom = "path", maptype = "terrain",
#'   color = I("red"), size = I(2), alpha = I(.5)
#' )
#'
#' trek_df <- trek(from, to, mode = "walking")
#' qmplot(lon, lat, data = trek_df, geom = "path", maptype = "terrain",
#'   color = I("red"), size = I(2), alpha = I(.5)
#' )
#'
#' trek_df <- trek(from, to, mode = "transit")
#' qmplot(lon, lat, data = trek_df, geom = "path", maptype = "terrain",
#'   color = I("red"), size = I(2), alpha = I(.5)
#' )
#'
#'
#'
#' ## neat faceting example
#' ########################################
#'
#' from <- "houston, texas"; to <- "waco, texas"
#' trek_df <- trek(from, to, alternatives = TRUE)
#'
#' qmplot(lon, lat, data = trek_df, geom = "path",
#'   color = route, size = I(2), maptype = "terrain",
#'   alpha = I(.5)
#' )
#'
#' qmplot(lon, lat, data = trek_df, geom = "path",
#'   color = route, size = I(2), maptype = "terrain",
#'   zoom = 8
#' ) + facet_grid(. ~ route)
#'
#'
#'
#'
#' }
#'
trek <- function (
  from,
  to,
  mode = c("driving","walking","bicycling", "transit"),
  output = c("simple","all"),
  alternatives = FALSE,
  units = "metric",
  urlonly = FALSE,
  override_limit = FALSE,
  ext = "com",
  inject = "",
  ...
) {

  # check parameters
  if(is.numeric(from) && length(from) == 2) from <- revgeocode(from)
  stopifnot(is.character(from))
  if(is.numeric(to) && length(to) == 2) to <- revgeocode(to)
  stopifnot(is.character(to))

  mode <- match.arg(mode)
  output <- match.arg(output)
  stopifnot(is.logical(alternatives))
  if (!has_google_key() && !urlonly) stop("Google now requires an API key.", "\n       See ?register_google for details.", call. = FALSE)


  # set url base
  url_base <- glue("https://maps.googleapis.{ext}/maps/api/directions/json?")


  # initialize the url query
  url_query_from <- from %>% str_trim() %>% str_replace_all(" +", "+") %>% c("origin" = .)
  url_query_to <- to %>% str_trim() %>% str_replace_all(" +", "+") %>% c("destination" = .)
  url_query <- c(url_query_from, url_query_to)


  # add google account stuff to query, if applicable
  url_query <- c(url_query, "client" = google_client(), "signature" = google_signature(), "key" = google_key())
  url_query <- url_query[!is.na(url_query)]


  # add mode and other stuff
  url_query <- c(url_query, "mode" = mode, "alternatives" = tolower(alternatives), "units" = units)


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


  # check/update google query limit
  # check_route_query_limit(url_string, elems = 1, override = override_limit)


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
        "http_500" = function(c) "HTTP 500 Internal Server Error",
        "http_503" = function(c) "HTTP 503 Service Unavailable - Server bogged down, try later"
      )
    )
    return(return_failed_trek(output))
  }


  # grab content
  tree <- httr::content(response)


  # return NA if zero results are found
  if (tree$status == "ZERO_RESULTS") {
    warning("No route was returned from Google.")
    return(return_failed_trek(output))
  }


  # return output = "all"
  if(output == "all") return(tree)


  # extract output from tree and format
  treks <- tree$routes %>%
    map(~ decode_google_route(.x$overview_polyline$points))


  # label routes
  for (k in seq_along(treks)) treks[[k]]$route <- LETTERS[k]


  # bind and return
  dplyr::bind_rows(treks)
}













return_failed_trek <- function (output) {
  if (output == "simple") {
    return(tibble(
      "lat" = NA_real_,
      "lon" = NA_real_,
      "route" = NA_character_
    ))
  } else if (output == "all") {
    return(list())
  }
}






















# the following is from @akmed (stackoverflow)
# see http://stackoverflow.com/questions/30270011/ggmap-route-finding-doesnt-stay-on-roads
# https://developers.google.com/maps/documentation/utilities/polylinealgorithm
decode_google_route <- function(encoded){

  vlen <- nchar(encoded)
  vindex <- 0
  varray <- NULL
  vlat <- 0
  vlng <- 0

  while(vindex < vlen){
    vb <- NULL
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen){
        vindex <- vindex + 1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63
      }

      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }

    dlat <- ifelse(bitAnd(vresult, 1), -(bitShiftR(vresult, 1)+1), bitShiftR(vresult, 1))
    vlat <- vlat + dlat

    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen) {
        vindex <- vindex+1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63
      }

      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }

    dlng <- ifelse(bitAnd(vresult, 1), -(bitShiftR(vresult, 1)+1), bitShiftR(vresult, 1))
    vlng <- vlng + dlng

    varray <- rbind(varray, c(vlat * 1e-5, vlng * 1e-5))
  }
  coords <- as_tibble(data.frame(varray))
  names(coords) <- c("lat", "lon")
  coords
}








