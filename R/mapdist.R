#' Compute map distances using Google
#'
#' Compute map distances using Google's Distance Matrix API. Note: To use
#' Google's Distance Matrix API, you must first enable the API in the Google
#' Cloud Platform Console. See \code{?register_google}.
#'
#' @param from name of origin addresses in a data frame (vector accepted)
#' @param to name of destination addresses in a data frame (vector accepted)
#' @param output amount of output
#' @param mode driving, bicycling, walking, or transit
#' @param urlonly return only the url?
#' @param override_limit override the current query count
#'   (.google_distance_query_times)
#' @param ext domain extension (e.g. "com", "co.nz")
#' @param inject character string to add to the url
#' @param ... ...
#' @return a data frame (output="simple") or all of the geocoded information
#'   (output="all")
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @details if parameters from and to are specified as geographic coordinates,
#'   they are reverse geocoded with revgeocode.  note that the google maps api
#'   limits to 2500 element queries a day.
#' @seealso
#'   \url{http://code.google.com/apis/maps/documentation/distancematrix/},
#'   \url{https://developers.google.com/maps/documentation/distance-matrix/intro}
#' @name mapdist
#' @export
#' @examples
#'
#' \dontrun{ requires Google API key, see ?register_google
#'
#' ## basic usage
#' ########################################
#'
#' mapdist("waco, texas", "houston, texas")
#'
#' from <- c("houston, texas", "dallas")
#' to <- "waco, texas"
#' mapdist(from, to)
#' mapdist(from, to, mode = "bicycling")
#' mapdist(from, to, mode = "walking")
#'
#' from <- c(
#'   "1600 Amphitheatre Parkway, Mountain View, CA",
#'   "3111 World Drive Walt Disney World, Orlando, FL"
#' )
#' to <- "1600 Pennsylvania Avenue, Washington DC"
#' mapdist(from, to)
#'
#' from <- "st lukes hospital houston texas"
#' to <- "houston zoo, houston texas"
#' mapdist(from, to, mode = "transit")
#'
#' from <- c("houston", "houston", "dallas")
#' to <- c("waco, texas", "san antonio", "houston")
#' mapdist(from, to)
#'
#'
#' ## geographic coordinates are accepted as well
#' ########################################
#' (wh <- as.numeric(geocode("the white house, dc")))
#' (lm <- as.numeric(geocode("lincoln memorial washington dc")))
#' mapdist(wh, lm, mode = "walking")
#'
#' }
#'










#' @rdname mapdist
#' @export
mapdist <- function (
  from,
  to,
  mode = c("driving","walking","bicycling","transit"),
  output = c("simple","all"),
  urlonly = FALSE,
  override_limit = FALSE,
  ext = "com",
  inject = "",
  ...
) {

  # check parameters
  if (is.numeric(from) && length(from) == 2) from <- revgeocode(from)
  stopifnot(is.character(from))
  if (is.numeric(to) && length(to) == 2) to <- revgeocode(to)
  stopifnot(is.character(to))

  from_to_df <- tibble("from" = from, "to" = to)
  mode <- match.arg(mode)
  output <- match.arg(output)

  if (!has_google_key() && !urlonly) stop("Google now requires an API key.", "\n       See ?register_google for details.", call. = FALSE)



  getdists <- function (df) {

    # set url base
    url_base <- glue("https://maps.googleapis.{ext}/maps/api/distancematrix/json?")

    # initialize the url query
    url_query_from <- df$from[1] %>% str_trim() %>% str_replace_all(" +", "+") %>% c("origins" = .)
    url_query_to <- df$to %>% str_trim() %>% str_replace_all(" +", "+") %>% str_c(collapse = "|") %>% c("destinations" = .)
    url_query <- c(url_query_from, url_query_to)

    # add google account stuff to query, if applicable
    url_query <- c(url_query, "client" = google_client(), "signature" = google_signature(), "key" = google_key())
    url_query <- url_query[!is.na(url_query)]

    # add mode and other stuff
    url_query <- c(url_query, "mode" = mode)

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

    # check if query is too long
    if(nchar(url) >= 2048){
      n <- nrow(df)
      half_df <- floor(n/2)
      return(
        bind_rows(
          getdists(df[half_df,]),
          getdists(df[(half_df+1L):n,])
        )
      )
    }

    # return early if user only wants url
    if(urlonly) if(showing_key()) return(url) else return(scrub_key(url))

    # hash for caching
    url_hash <- digest::digest(url)

    # check/update google query limit
    # check_dist_query_limit(url_string, elems = nrow(df), override = override_limit)

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
      return(return_failed_mapdist(output))
    }

    # grab content
    tree <- httr::content(response)

    # label destinations - first check if all were found
    if (length(df$to) != length(tree$destination_addresses)){
      message("Matching was not perfect, returning all.")
      names( tree$rows[[c(1,1)]] ) <- tree$destination_addresses
      output <<- "all"
    } else {
      names( tree$rows[[c(1,1)]] ) <- df$to
    }

    # return
    tree$rows[[c(1,1)]]
  }

  out <- split(from_to_df, from_to_df$from) %>% map(~ getdists(.x))

  # return all
  if(output == "all") return(out)

  # grab interesting parts, format, and return
  out %>%
    rev() %>%
    map(function (origin) {
      origin %>%
        map(~
          tibble(
            "m" = .x$distance$value,
            "km" = .x$distance$value/1000,
            "miles" = 0.0006214 * .x$distance$value,
            "seconds" = .x$duration$value,
            "minutes" = .x$duration$value / 60,
            "hours" = .x$duration$value / 3600
          )
        )
    }) %>%
      flatten() %>%
      bind_rows() %>%
      bind_cols(from_to_df, .) %>%
      mutate("mode" = mode)

}

















check_dist_query_limit <- function(url, queries_sought, override){

  if(exists(".google_distance_query_times", ggmap_environment)){

    .google_distance_query_times <- get(".google_distance_query_times", envir = ggmap_environment)

    queries_used_in_last_second <- with(.google_distance_query_times, sum(queries[time >= Sys.time() - 1L]))

    if (!override && (queries_used_in_last_second + queries_sought > google_second_limit())) Sys.sleep(.2) # can do better

    assign(
      ".google_distance_query_times",
      bind_rows(.google_distance_query_times, tibble("time" = Sys.time(), "url" = url, "queries" = queries_sought)),
      envir = ggmap_environment
    )


  } else {

    assign(
      ".google_distance_query_times",
      tibble("time" = Sys.time(), "url" = url, "queries" = queries_sought),
      envir = ggmap_environment
    )

  }
}







#' @rdname mapdist
#' @export
distQueryCheck <- function(){

  .Deprecated(msg = "As of mid-2018, Google no longer has daily query limits.")
  queries <- NA; rm(queries)

  if(exists(".google_distance_query_times", ggmap_environment)){

    .google_distance_query_times <- get(".google_distance_query_times", envir = ggmap_environment)

    google_distance_queries_in_last_24hrs <-
      .google_distance_query_times %>%
        dplyr::filter(time >= Sys.time() - 24L*60L*60L) %>%
        dplyr::select(queries) %>%
        sum()

    remaining <- google_day_limit() - google_distance_queries_in_last_24hrs
    message(remaining, " Google Distance Matrix API queries remaining.")

  } else {

  	remaining <- google_day_limit()
    message(remaining, " Google Distance Matrix API queries remaining.")

  }

  invisible(remaining)
}









return_failed_mapdist <- function (output) {
  if (output == "simple") {
    return(tibble(
      "m" = NA_real_,
      "km" = NA_real_,
      "miles" = NA_real_,
      "seconds" = NA_real_,
      "minutes" = NA_real_,
      "hours" = NA_real_
    ))
  } else if (output == "all") {
    return(list())
  }
}

