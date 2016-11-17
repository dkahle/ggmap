#' Grab a trek from Google
#'
#' Grab a trek from Google that will plot over the roadways. Note
#' that in most cases by using this function you are agreeing to the
#' Google Maps API Terms of Service at
#' https://developers.google.com/maps/terms.
#'
#' @param from name of origin addresses in a data frame (vector
#'   accepted)
#' @param to name of destination addresses in a data frame (vector
#'   accepted)
#' @param output amount of output
#' @param mode driving, bicycling, walking, or transit
#' @param alternatives should more than one route be provided?
#' @param units "metric"
#' @param messaging turn messaging on/off
#' @param urlonly return only the url?
#' @param override_limit override the current query count
#'   (.GoogleRouteQueryCount)
#' @param ext domain extension (e.g. "com", "co.nz")
#' @param inject character string to add to the url
#' @param ... ...
#' @return a data frame (output="simple") or all of the geocoded
#'   information (output="all")
#' @author David Kahle \email{david.kahle@@gmail.com} with the key
#'   decoding algorithm due to stackoverflow user akhmed
#' @seealso
#' \url{https://developers.google.com/maps/documentation/directions/},
#' \url{http://stackoverflow.com/questions/30270011/ggmap-route-finding-doesnt-stay-on-roads},
#' \code{\link{route}}, \code{\link{routeQueryCheck}}
#' \code{\link{register_google}}
#' @export
#' @examples
#'
#' \dontrun{ # to cut down on check time
#'
#' from <- "houston, texas"
#' to <- "waco, texas"
#' route_df <- route(from, to, structure = "route")
#' trek_df <- trek(from, to, structure = "route")
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
#' from <- "houston, texas"; to <- "waco, texas"
#' trek_df <- trek(from, to, alternatives = TRUE)
#' qmplot(lon, lat, data = trek_df, geom = "path",
#'   color = route, size = I(2), alpha = I(.5)
#' )
#'
#'
#' from <- "los angeles, california"; to <- "new york, new york"
#' trek_df <- trek(from, to, alternatives = TRUE)
#' qmplot(lon, lat, data = trek_df, geom = "path",
#'   color = route, size = I(2), maptype = "terrain",
#'   zoom = 5
#' ) + facet_wrap(~ route, ncol = 1)
#'
#'
#'
#'
#' }
#'
trek <- function(from, to, mode = c("driving","walking","bicycling", "transit"),
  output = c("simple","all"), alternatives = FALSE, units = "metric",
  messaging = FALSE, urlonly = FALSE, override_limit = FALSE,
  ext = "com", inject = "", ...)
{

  # check parameters
  if(is.numeric(from) && length(from) == 2) from <- revgeocode(from)
  stopifnot(is.character(from))
  if(is.numeric(to) && length(to) == 2) to <- revgeocode(to)
  stopifnot(is.character(to))
  mode <- match.arg(mode)
  output <- match.arg(output)
  stopifnot(is.logical(alternatives))
  stopifnot(is.logical(messaging))


  # format url
  origin <- URLencode(from, reserved = TRUE)
  destination <- URLencode(to, reserved = TRUE)
  posturl <- paste(fmteq(origin), fmteq(destination), fmteq(mode), fmteq(units),
    fmteq(alternatives, tolower), sep = "&"
  )

  # add google account stuff
  if (has_goog_client() && has_goog_signature()) {
    client <- goog_client()
    signature <- goog_signature()
    posturl <- paste(posturl, fmteq(client), fmteq(signature), sep = "&")
  } else if (has_goog_key()) {
    key <- goog_key()
    posturl <- paste(posturl, fmteq(key), sep = "&")
  }

  # construct url
  url_string <- paste0(
    sprintf("https://maps.googleapis.%s/maps/api/directions/json?", ext),
    posturl
  )

  # inject any remaining stuff
  if(inject != "") url_string <- paste(url_string, inject, sep = "&")

  # encode
  url_string <- URLencode( enc2utf8(url_string) )
  if(urlonly) return(url_string)


  # check/update google query limit
  check_route_query_limit(url_string, elems = 1,
    override = override_limit, messaging = messaging)


  # distance lookup
  if(messaging) message("trying url ", url_string)
  connect <- url(url_string)
  tree <- fromJSON(paste(readLines(connect), collapse = ""))
  close(connect)

  # return output = "all"
  if(output == "all") return(tree)

  # return NA if zero results are found
  if (tree$status == "ZERO_RESULTS") {
    warning("No route was returned from Google.")
    return(NA)
  }

  # message user
  message("Source : ", url_string)

  # extract output from tree and format
  treks <- llply(tree$routes, function(route){
    decode_google_route( route$overview_polyline$points )
  })

  # label routes
  for (k in seq_along(treks)) {
    treks[[k]]$route <- LETTERS[k]
  }

  # bind and return
  dplyr::bind_rows(treks)
}



































# the following is from @akmed (stackoverflow)
# see http://stackoverflow.com/questions/30270011/ggmap-route-finding-doesnt-stay-on-roads
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
  coords <- data.frame(varray)
  names(coords) <- c("lat", "lon")
  coords
}








