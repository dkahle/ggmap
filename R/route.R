#' Grab a route from Google
#'
#' Route two locations: determine a sequence of steps (legs) between two
#' locations using the Google Directions API. Note: To use Google's Directions
#' API, you must first enable the API in the Google Cloud Platform Console. See
#' [register_google()].
#'
#' @param from vector of origin addresses
#' @param to vector of destination addresses
#' @param output amount of output ("simple" or "all")
#' @param structure structure of output, "legs" or "route", see examples
#' @param mode driving, bicycling, walking, or transit
#' @param alternatives should more than one route be provided?
#' @param units "metric"
#' @param urlonly return only the url?
#' @param override_limit override the current query count
#' @param ext domain extension (e.g. "com", "co.nz")
#' @param inject character string to add to the url
#' @param ... ...
#' @return a data frame (output="simple") or all of the geocoded information
#'   (output="all")
#' @author David Kahle \email{david@@kahle.io}
#' @seealso \url{https://developers.google.com/maps/documentation/directions/},
#'   [trek()], [legs2route()], [geom_leg()], [register_google()]
#' @name route
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
#' route(from, to, structure = "legs")
#' route(from, to, structure = "route")
#'
#' route(from, to, alternatives = TRUE)
#'
#'
#' ## comparison to trek
#' ########################################
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
#' qmap("college station, texas", zoom = 6) +
#'   geom_path(
#'     aes(x = lon, y = lat), colour = "red", size = 1.5,
#'     data = route_df, lineend = "round"
#'   )
#'
#'
#'
#'
#'
#' }
#'








#' @rdname route
#' @export
route <- function (
  from,
  to,
  mode = c("driving","walking","bicycling", "transit"),
  structure = c("legs","route"),
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
  structure <- match.arg(structure)
  if (!has_google_key() && !urlonly) {
    cli::cli_abort("Google now requires an API key; see {.fn ggmap::register_google}.")
  }


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
  url <- str_replace_all(url, "#", "%23") # selectively url-encode


  # return early if user only wants url
  if(urlonly) if(showing_key()) return(url) else return(scrub_key(url))


  # hash for caching
  url_hash <- digest::digest(scrub_key(url))


  # check/update google query limit
  # check_route_query_limit(url_string, elems = 1, override = override_limit)


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
    return(return_failed_route(output))
  }


  # grab content
  tree <- httr::content(response)


  # return NA if zero results are found
  if (tree$status == "ZERO_RESULTS") {
    cli::cli_warn("No route was returned from Google.")
    return(return_failed_route(output))
  }


  # return output = "all"
  if(output == "all") return(tree)


  # extract output from tree and format
  out <- map(tree$routes, function (route) {

    route %>%
      pluck("legs", 1L, "steps") %>%
      map(~ tibble(
        "m"        = .x$distance$value,
        "km"       = .x$distance$value/1000,
        "miles"    = 0.0006214 * .x$distance$value,
        "seconds"  = .x$duration$value,
        "minutes"  = .x$duration$value / 60,
        "hours"    = .x$duration$value / 3600,
        "start_lon" = .x$start_location$lng,
        "start_lat" = .x$start_location$lat,
        "end_lon"   = .x$end_location$lng,
        "end_lat"   = .x$end_location$lat
      )) %>%
      bind_rows()

  }) %>% imap(~ {
    df <- .x
    df$route <- LETTERS[.y]
    df
  }) %>% bind_rows()

  # return output = "simple"
  if(structure == "legs"){
    return(out)
  } else {
  	return(legs2route(out))
  }
}









return_failed_route <- function (output) {
  if (output == "simple") {
   return(tibble(
     "m"        = NA_real_,
     "km"       = NA_real_,
     "miles"    = NA_real_,
     "seconds"  = NA_real_,
     "minutes"  = NA_real_,
     "hours"    = NA_real_,
     "start_lon" = NA_real_,
     "start_lat" = NA_real_,
     "end_lon"   = NA_real_,
     "end_lat"   = NA_real_
   ))
  } else if (output == "all") {
    return(list())
  }
}

















check_route_query_limit <- function(url, queries_sought, override){


  if(exists(".google_route_query_times", ggmap_environment)){

    .google_route_query_times <- get(".google_route_query_times", envir = ggmap_environment)

    queries_used_in_last_second <- with(.google_route_query_times, sum(queries[time >= Sys.time() - 1L]))

    if (!override && (queries_used_in_last_second + queries_sought > google_second_limit())) Sys.sleep(.2) # can do better

    assign(
      ".google_route_query_times",
      bind_rows(.google_route_query_times, tibble("time" = Sys.time(), "url" = url, "queries" = queries_sought)),
      envir = ggmap_environment
    )


  } else {

    assign(
      ".google_route_query_times",
      tibble("time" = Sys.time(), "url" = url, "queries" = queries_sought),
      envir = ggmap_environment
    )

  }
  #
  #
  # if(exists(".GoogleRouteQueryCount", .GlobalEnv)){
  #
  #   .GoogleRouteQueryCount <<- dplyr::filter(.GoogleRouteQueryCount, time >= Sys.time() - 24*60*60)
  #
  #   # limit per 24 hours
  #   dayQueriesUsed <- sum(.GoogleRouteQueryCount$elements)
  #   if(dayQueriesUsed + queries_sought > google_day_limit()){
  #     cli::cli_alert_warning("Query max exceeded, see {.fn ggmap::route}. Current total = {dayQueriesUsed}")
  #     if(!override) return("stop")
  #   }
  #
  #   # limit per second
  #   secondQueriesUsed <- with(.GoogleRouteQueryCount, sum(elements[time >= Sys.time() - 1]))
  #   if(secondQueriesUsed + queries_sought > google_second_limit()){
  #     message(".", appendLF = FALSE)
  #     Sys.sleep(.2) # can do better
  #   }
  #
  #
  #   # append to .GoogleRouteQueryCount
  #   if(length(grep("transit", url)) == 1){ # a transit request
  #     tmp <- data.frame(time = Sys.time(),  url = url, elements = queries_sought, stringsAsFactors = FALSE)
  #     tmp <- bind_rows(tmp, tmp, tmp, tmp)
  #     .GoogleRouteQueryCount <<- rbind(.GoogleRouteQueryCount, tmp)
  #   } else {
  #     .GoogleRouteQueryCount <<- rbind(.GoogleRouteQueryCount,
  #       data.frame(time = Sys.time(),  url = url, elements = queries_sought, stringsAsFactors = FALSE)
  #     )
  #   }
  #
  #
  #
  # } else {
  #
  #   .GoogleRouteQueryCount <<- data.frame(
  #     time = Sys.time(),  url = url, elements = queries_sought, stringsAsFactors = FALSE
  #   )
  #
  # }
}















#' @rdname route
#' @export
routeQueryCheck <- function(){

  .Deprecated(msg = "As of mid-2018, Google no longer has daily query limits.")
  queries <- NA; rm(queries)


  if(exists(".google_route_query_times", ggmap_environment)){

    .google_route_query_times <- get(".google_route_query_times", envir = ggmap_environment)

    google_route_queries_in_last_24hrs <-
      .google_route_query_times %>%
      dplyr::filter(time >= Sys.time() - 24L*60L*60L) %>%
      dplyr::select(queries) %>%
      sum()

    remaining <- google_day_limit() - google_route_queries_in_last_24hrs
    cli::cli_alert_info("{remaining} Google Directions API queries remaining.")

  } else {

    remaining <- google_day_limit()
    cli::cli_alert_info("{remaining} Google Directions API queries remaining.")

  }


  invisible(remaining)

}

















#' Convert a leg-structured route to a route-structured route
#'
#' Convert a leg-structured route to a route-structured route
#'
#' @param legsdf a legs-structured route, see [route()]
#' @seealso [geom_path()]
#' @export
#' @name legs2route
#' @examples
#'
#' \dontrun{ requires Google API key, see ?register_google
#'
#' (legs_df <- route("houston","galveston"))
#' legs2route(legs_df)
#'
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
#'     aes(x = start_lon, y = start_lat, xend = end_lon, yend = end_lat),
#'     colour = "red", size = 1.5, data = legs_df
#'   )
#' # notice boxy ends
#'
#' qmap("college station, texas", zoom = 8) +
#'   geom_leg(
#'     aes(x = start_lon, y = start_lat, xend = end_lon, yend = end_lat),
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
  data <- NA; rm(data)

  legsdf %>%
    group_by(route) %>%
    nest() %>%
    mutate(data = map(data, function (leg) {
      out <- leg[,-which(names(leg) %in% c("start_lon","start_lat","end_lon","end_lat"))]
      out$lon <- leg$start_lon
      out$lat <- leg$start_lat
      out <- rbind(out, NA)
      out$lon[nrow(out)] <- tail(leg$end_lon,1)
      out$lat[nrow(out)] <- tail(leg$end_lat,1)
      out
    })) %>%
    unnest(cols = "data")

}














