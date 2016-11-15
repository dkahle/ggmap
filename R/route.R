#' Grab a route from Google
#'
#' Grab a route from Google. Note that in most cases by using this
#' function you are agreeing to the Google Maps API Terms of Service
#' at https://developers.google.com/maps/terms.
#'
#' @param from name of origin addresses in a data frame (vector
#'   accepted)
#' @param to name of destination addresses in a data frame (vector
#'   accepted)
#' @param output amount of output
#' @param structure structure of output, see examples
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
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso
#' \url{https://developers.google.com/maps/documentation/directions/},
#' \code{\link{trek}}, \code{\link{legs2route}},
#' \code{\link{routeQueryCheck}}, \code{\link{geom_leg}},
#' \code{\link{register_google}}
#' @export
#' @examples
#'
#' \dontrun{ # to cut down on check time
#'
#' from <- "houston, texas"
#' to <- "waco, texas"
#' route_df <- route(from, to, structure = "route")
#' trek_df  <-  trek(from, to, structure = "route")
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
#' qmap("college station, texas", zoom = 6) +
#'   geom_path(
#'     aes(x = lon, y = lat), colour = "red", size = 1.5,
#'     data = route_df, lineend = "round"
#'   )
#'
#' routeQueryCheck()
#'
#'
#'
#' }
#'
route <- function(from, to, mode = c("driving","walking","bicycling", "transit"),
  structure = c("legs","route"), output = c("simple","all"),
  alternatives = FALSE, units = "metric", messaging = FALSE,
  urlonly = FALSE, override_limit = FALSE,
  ext = "com", inject = "", ...)
{

  # check parameters
  if(is.numeric(from) && length(from) == 2) from <- revgeocode(from)
  stopifnot(is.character(from))
  if(is.numeric(to) && length(to) == 2) to <- revgeocode(to)
  stopifnot(is.character(to))
  mode <- match.arg(mode)
  structure <- match.arg(structure)
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
  check_route_query_limit(url_string, elems = 1, override = override_limit, messaging = messaging)


  # distance lookup
  if(messaging) message("trying url ", url_string)
  connect <- url(url_string); on.exit(close(connect), add = TRUE)
  tree <- fromJSON(paste(readLines(connect), collapse = ""))


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
  out <- ldply(tree$routes, function(route){

    route_df <- ldply(route$legs[[1]]$steps, function(oneLegList){
      data.frame(
        m        = oneLegList$distance$value,
        km       = oneLegList$distance$value/1000,
        miles    = 0.0006214 * oneLegList$distance$value,
        seconds  = oneLegList$duration$value,
        minutes  = oneLegList$duration$value / 60,
  	    hours    = oneLegList$duration$value / 3600,
  	    startLon = oneLegList$start_location$lng,
  	    startLat = oneLegList$start_location$lat,
  	    endLon   = oneLegList$end_location$lng,
  	    endLat   = oneLegList$end_location$lat
      )
    })
    route_df$leg <- 1:nrow(route_df)
    route_df
  })

  # label routes
  stepsPerRoute <- vapply(tree$routes,
    function(route) length(route$legs[[1]]$steps),
    numeric(1)
  )

  nRoutes <- length(stepsPerRoute)
  routeLabel <- NULL
  for(k in 1:nRoutes){
    routeLabel <- c(routeLabel, rep(LETTERS[k], stepsPerRoute[k]))
  }
  if(nRoutes > 1) out$route <- routeLabel

  # return output = "simple"
  if(structure == "legs"){
    return(out)
  } else {
  	return(legs2route(out))
  }
}











check_route_query_limit <- function(url_string, elems, override, messaging){
  .GoogleRouteQueryCount <- NULL; rm(.GoogleRouteQueryCount); # R CMD check trick

  if(exists(".GoogleRouteQueryCount", .GlobalEnv)){

    .GoogleRouteQueryCount <<- dplyr::filter(.GoogleRouteQueryCount, time >= Sys.time() - 24*60*60)

    # limit per 24 hours
    dayQueriesUsed <- sum(.GoogleRouteQueryCount$elements)
    if(dayQueriesUsed + elems > goog_day_limit()){
      message("query max exceeded, see ?route  current total = ", dayQueriesUsed)
      if(!override) return("stop")
    }

    # limit per second
    secondQueriesUsed <- with(.GoogleRouteQueryCount, sum(elements[time >= Sys.time() - 1]))
    if(secondQueriesUsed + elems > goog_second_limit()){
      message(".", appendLF = FALSE)
      Sys.sleep(.2) # can do better
    }


    # append to .GoogleRouteQueryCount
    if(length(grep("transit", url_string)) == 1){ # a transit request
      tmp <- data.frame(time = Sys.time(),  url = url_string, elements = elems, stringsAsFactors = FALSE)
      tmp <- bind_rows(tmp, tmp, tmp, tmp)
      .GoogleRouteQueryCount <<- rbind(.GoogleRouteQueryCount, tmp)
    } else {
      .GoogleRouteQueryCount <<- rbind(.GoogleRouteQueryCount,
        data.frame(time = Sys.time(),  url = url_string, elements = elems, stringsAsFactors = FALSE)
      )
    }



  } else {

    .GoogleRouteQueryCount <<- data.frame(
      time = Sys.time(),  url = url_string, elements = elems, stringsAsFactors = FALSE
    )

  }
}















#' Check Google Maps Directions API query limit
#'
#' Check Google Maps Directions API query limit
#'
#' @return a data frame
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \url{https://developers.google.com/maps/documentation/directions/}
#' @export
#' @examples
#' \dontrun{
#' routeQueryCheck()
#' }
routeQueryCheck <- function(){

  .GoogleRouteQueryCount <- NULL; rm(.GoogleRouteQueryCount);

  if(exists(".GoogleRouteQueryCount", .GlobalEnv)){

  	remaining <- goog_day_limit() - sum(
  	  dplyr::filter(.GoogleRouteQueryCount, time >= Sys.time() - 24*60*60)$elements
  	)
    message(remaining, " routing queries remaining.")

  } else {

  	remaining <- goog_day_limit()
    message(remaining, " routing queries remaining.")

  }

  invisible(remaining)

}





#' Single line segments with rounded ends
#'
#' Single line segments with rounded ends
#'
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param arrow arrow
#' @param ... ...
#' @seealso geom_segment in ggplot2, inspired by
#'   \url{http://spatialanalysis.co.uk/2012/02/great-maps-ggplot2/},
#'   \code{\link{route}}
#' @details only intended for use in ggmap package.  only designed
#'   for mercator projection.
#' @export
#' @examples
#'
#' \dontrun{ # removed for R CMD check speed
#'
#' map <- get_map(
#'   location = c(-77.0425, 38.8925), # painfully picked by hand
#'   source = "google", zoom = 14, maptype = "satellite"
#' )
#' ggmap(map)
#'
#'
#' (legs_df <- route(
#'   "the white house, dc",
#'   "lincoln memorial washington dc",
#'   alternatives = TRUE
#' ))
#'
#' ggplot(data = legs_df) +
#'   geom_leg(aes(
#'     x = startLon, xend = endLon,
#'     y = startLat, yend = endLat
#'   )) +
#'   coord_map()
#'
#' ggplot(data = legs_df) +
#'   geom_leg(aes(
#'     x = startLon, xend = endLon,
#'     y = startLat, yend = endLat,
#'     color = route
#'   )) +
#'   coord_map()
#'
#'
#' ggmap(map) +
#'   geom_leg(
#'     aes(
#'       x = startLon, xend = endLon,
#'       y = startLat, yend = endLat
#'     ),
#'     data = legs_df, color = "red"
#'   )
#'
#' # adding a color aesthetic errors because of a base-layer problem
#' # ggmap(map) +
#' #   geom_leg(
#' #     aes(
#' #       x = startLon, xend = endLon,
#' #       y = startLat, yend = endLat,
#' #       color = route
#' #   )
#' # )
#'
#'
#' # this is probably the easiest hack to fix it
#' ggplot(data = legs_df) +
#'   inset_ggmap(map) +
#'   geom_leg(
#'     aes(
#'       x = startLon, xend = endLon,
#'       y = startLat, yend = endLat,
#'       color = route
#'     ),
#'     data = legs_df
#'   ) +
#'   coord_map()
#'
#' }
#'
geom_leg <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", arrow = NULL, lineend = "round",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomSegment,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      lineend = lineend,
      na.rm = na.rm,
      ...
    )
  )
}


















#' Convert a leg-structured route to a route-structured route
#'
#' Convert a leg-structured route to a route-structured route
#'
#' @param legsdf a legs-structured route, see \code{\link{route}}
#' @seealso geom_path in ggplot2
#' @export
#' @examples
#'
#' \dontrun{
#'
#' (legs_df <- route("houston","galveston"))
#' legs2route(legs_df)
#
#' (legs_df <- route(
#'   "marrs mclean science, baylor university",
#'   "220 south 3rd street, waco, tx 76701", # ninfa"s
#'   alternatives = TRUE))
#'
#' legs2route(legs_df)
#'
#'
#'
#'
#' from <- "houston, texas"
#' to <- "waco, texas"
#' legs_df <- route(from, to)
#'
#'
#' qmap("college station, texas", zoom = 8) +
#'   geom_segment(
#'     aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
#'     colour = "red", size = 1.5, data = legs_df
#'   )
#' # notice boxy ends
#'
#' qmap("college station, texas", zoom = 8) +
#'   geom_leg(
#'     aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
#'     colour = "red", size = 1.5, data = legs_df
#'   )
#' # notice overshooting ends
#'
#' route_df <- legs2route(legs_df)
#' qmap("college station, texas", zoom = 8) +
#'   geom_path(
#'     aes(x = lon, y = lat),
#'     colour = "red", size = 1.5, data = route_df, lineend = "round"
#'   )
#'
#'
#'
#' }
#'
legs2route <- function(legsdf){

  if(!("route" %in% names(legsdf))) legsdf$route <- "A"

  out <- ddply(legsdf, .(route), function(df){
  	out <- df[,-which(names(df) %in% c("startLon","startLat","endLon","endLat"))]
    out$lon <- df$startLon
    out$lat <- df$startLat
    out <- rbind(out, NA)
    out$lon[nrow(out)] <- tail(df$endLon,1)
    out$lat[nrow(out)] <- tail(df$endLat,1)
    out$route[nrow(out)] <- tail(df$route,1)
    out
  })

  if(length(unique(legsdf$route)) == 1) out <- out[,-which(names(out) == "route")]
  out
}




















