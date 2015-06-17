#' Grab a route from Google
#'
#' Grab a route from Google. Note that in most cases by using this function you are agreeing to the Google Maps API Terms of Service at https://developers.google.com/maps/terms.
#'
#' @param from name of origin addresses in a data frame (vector accepted)
#' @param to name of destination addresses in a data frame (vector accepted)
#' @param output amount of output
#' @param structure structure of output, see examples
#' @param mode driving, bicycling, walking, or transit
#' @param alternatives should more than one route be provided?
#' @param messaging turn messaging on/off
#' @param sensor whether or not the geocoding request comes from a device with a location sensor
#' @param override_limit override the current query count (.GoogleRouteQueryCount)
#' @return a data frame (output="simple") or all of the geocoded information (output="all")
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \url{https://developers.google.com/maps/documentation/directions/}, \code{\link{legs2route}}, \code{\link{routeQueryCheck}}, \code{\link{geom_leg}}
#' @export
#' @examples
#'
#' \dontrun{ # to cut down on check time
#'
#' from <- "houson, texas"
#' to <- "waco, texas"
#' route_df <- route(from, to, structure = "route")
#' qmap("college station, texas", zoom = 8) +
#'   geom_path(
#'     aes(x = lon, y = lat),  colour = "red", size = 1.5,
#'     data = route_df, lineend = "round"
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
#' (legs_df <- route(
#'   "marrs mclean science, baylor university",
#'   "220 south 3rd street, waco, tx 76701", # ninfa"s
#'   alternatives = TRUE))
#'
#' qmap("424 clay avenue, waco, tx", zoom = 15, maprange = TRUE, maptype = "hybrid",
#'     base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df)) +
#'   geom_leg(
#'     aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = route),
#'     alpha = 3/4, size = 2, data = legs_df
#'   ) +
#'   scale_x_continuous(breaks = pretty(c(-97.1325,-97.119),4), lim = c(-97.1325,-97.119)) +
#'   facet_wrap(~ route) + theme_bw() +
#'   labs(x = "Longitude", y = "Latitude", colour = "Routes")
#'
#' }
#'
route <- function(from, to, mode = c("driving","walking","bicycling", "transit"),
  structure = c("legs","route"), output = c("simple","all"), alternatives = FALSE,
  messaging = FALSE, sensor = FALSE, override_limit = FALSE)
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
  stopifnot(is.logical(sensor))

  # format url
  origin <- from
  origin <- gsub(" ", "+", origin)
  origin <- paste("origin=", origin, sep = "")
  destination <- to
  destination <- gsub(" ", "+", destination)
  destination <- paste("destination=", destination, sep = "")
  mode4url <- paste("mode=", mode, sep = "")
  unit4url <- paste("units=", "metric", sep = "")
  alts4url <- paste("alternatives=", tolower(as.character(alternatives)), sep = "")
  sensor4url <- paste("sensor=", tolower(as.character(sensor)), sep = "")
  posturl <- paste(origin, destination, mode4url, unit4url, alts4url, sensor4url, sep = "&")
  url_string <- paste("http://maps.googleapis.com/maps/api/directions/json?", posturl, sep = "")
  url_string <- URLencode(url_string)

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


  # message user
  message(paste0("Information from URL : ", url_string))


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
  stepsPerRoute <-
    sapply(tree$routes, function(route) length(route$legs[[1]]$steps))

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

    .GoogleRouteQueryCount <<-
      subset(.GoogleRouteQueryCount, time >= Sys.time() - 24*60*60)

    # 2500 per 24 hours
    if(sum(.GoogleRouteQueryCount$elements) + elems > 2500){
      message("query max exceeded, see ?route.  current total = ",
        sum(.GoogleRouteQueryCount$elements))
      if(!override) stop("google query limit exceeded.", call. = FALSE)
    }

    # 100 per 10 seconds
    if(with(.GoogleRouteQueryCount,
      sum(elements[time >= Sys.time() - 10]) + elems > 100
    )){
      if(messaging) message("waiting 10 seconds for another 100 queries...", appendLF=F)
      Sys.sleep(10) # can do better
      if(messaging) message(" done")
    }

    # append to .GoogleRouteQueryCount
    if(length(grep("transit", url_string)) == 1){ # a transit request
      tmp <- data.frame(time = Sys.time(),  url = url_string,
          elements = elems, stringsAsFactors = FALSE)
      tmp <- rbind(tmp, tmp, tmp, tmp)
      .GoogleRouteQueryCount <<- rbind(.GoogleRouteQueryCount,
        tmp
      )
    } else {
      .GoogleRouteQueryCount <<- rbind(.GoogleRouteQueryCount,
        data.frame(time = Sys.time(),  url = url_string,
          elements = elems, stringsAsFactors = FALSE)
      )
    }



  } else {

    .GoogleRouteQueryCount <<-
      data.frame(time = Sys.time(),  url = url_string,
        elements = elems, stringsAsFactors = FALSE)

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
  .GoogleRouteQueryCount <- NULL; rm(.GoogleRouteQueryCount); # R CMD check trick
  if(exists(".GoogleRouteQueryCount", .GlobalEnv)){
  	remaining <- 2500-sum(
  	  subset(.GoogleRouteQueryCount, time >= Sys.time() - 24*60*60)$elements
  	  )
    message(remaining, " route queries remaining.")
  } else {
  	remaining <- 2500
    message(remaining, " route queries remaining.")
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
#' @seealso geom_segment in ggplot2, inspired by \url{http://spatialanalysis.co.uk/2012/02/great-maps-ggplot2/}, \code{\link{route}}
#' @details only intended for use in ggmaps package.  only designed for mercator projection.
#' @export
#' @examples
#'
#' \dontrun{
#'
#' (legs_df <- route(
#' "marrs mclean science, baylor university",
#'   "220 south 3rd street, waco, tx 76701", # ninfa"s
#'   alternatives = TRUE))
#'
#' options("device")$device(width = 11.65, height = 4.17)
#' qmap("424 clay avenue, waco, tx", zoom = 16, maprange = TRUE, maptype = "satellite",
#'   base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df)) +
#'   geom_segment(
#'     aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = route),
#'     alpha = 3/4, size = 3, data = legs_df
#'   ) +
#'   scale_x_continuous(breaks = pretty(c(-97.1325,-97.119),4), lim = c(-97.1325,-97.119)) +
#'   facet_wrap(~ route) + theme_bw() +
#'   labs(x = "Longitude", y = "Latitude", colour = "Routes")
#'
#' qmap("424 clay avenue, waco, tx", zoom = 16, maprange = TRUE, maptype = "satellite",
#'   base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df)) +
#'   geom_leg(
#'     aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = route),
#'     alpha = 3/4, size = 2, data = legs_df
#'   ) +
#'   scale_x_continuous(breaks = pretty(c(-97.1325,-97.119),4), lim = c(-97.1325,-97.119)) +
#'   facet_wrap(~ route) + theme_bw() +
#'   labs(x = "Longitude", y = "Latitude", colour = "Routes")
#'
#'
#' }
#'
geom_leg <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", arrow = NULL, ...) {
  GeomLeg$new(mapping = mapping, data = data, stat = stat, position = position, arrow = arrow, ...)
}

GeomLeg <- proto(ggplot2:::GeomSegment, {
  objname <- "leg"

  draw <- function(., data, scales, coordinates, arrow=NULL, ...) {

    coords <- coord_transform(coordinates, data, scales)
    coords <- within(coords,{
      xend <- rescale(xend, 0:1, scales$x.range)
      yend <- rescale(yend, 0:1, scales$y.range)
    })

    with(coords,
      segmentsGrob(x, y, xend, yend, default.units="native",
      gp = gpar(col=alpha(colour, alpha), lwd=size * .pt,
        lty=linetype, lineend = "round"),
      arrow = arrow)
    )
  }
})



















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
#' from <- "houson, texas"
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














