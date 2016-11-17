#' Compute map distances using Google
#'
#' Compute map distances using Google Maps. Note that in most cases
#' by using this function you are agreeing to the Google Maps API
#' Terms of Service at https://developers.google.com/maps/terms.
#'
#' @param from name of origin addresses in a data frame (vector
#'   accepted)
#' @param to name of destination addresses in a data frame (vector
#'   accepted)
#' @param output amount of output
#' @param mode driving, bicycling, or walking
#' @param messaging turn messaging on/off
#' @param language language
#' @param urlonly return only the url?
#' @param override_limit override the current query count
#'   (.GoogleDistQueryCount)
#' @param ext domain extension (e.g. "com", "co.nz")
#' @param inject character string to add to the url
#' @param ... ...
#' @return a data frame (output="simple") or all of the geocoded
#'   information (output="all")
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @details if parameters from and to are specified as geographic
#'   coordinates, they are reverse geocoded with revgeocode.  note
#'   that the google maps api limits to 2500 element queries a day.
#' @seealso
#' \url{http://code.google.com/apis/maps/documentation/distancematrix/}
#' @export
#' @examples
#'
#' \dontrun{# online queries draw R CMD check times
#'
#' mapdist("waco, texas", "houston, texas")
#'
#' from <- c("houston, texas", "dallas")
#' to <- "waco, texas"
#' mapdist(from, to)
#' mapdist(from, to, mode = "bicycling")
#' mapdist(from, to, mode = "walking")
#'
#' # google requires a key for mode="transit"
#' register_google(key = "[your key here]")
#' from <- "city hall houston texas"
#' to <- "nrg stadium houston texas"
#' mapdist(from, to, mode = "transit")
#'
#' from <- c("houston", "houston", "dallas")
#' to <- c("waco, texas", "san antonio", "houston")
#' mapdist(from, to)
#'
#'
#' # geographic coordinates are accepted as well
#' (wh <- as.numeric(geocode("the white house, dc")))
#' (lm <- as.numeric(geocode("lincoln memorial washington dc")))
#' mapdist(wh, lm, mode = "walking")
#' distQueryCheck()
#'
#' }
#'
mapdist <- function(from, to, mode = c("driving","walking","bicycling","transit"),
  output = c("simple","all"), messaging = FALSE,
  language = "en-EN", urlonly = FALSE, override_limit = FALSE,
  ext = "com", inject = "", ...)
{

  # check parameters
  if(is.numeric(from) && length(from) == 2) from <- revgeocode(from)
  stopifnot(is.character(from))
  if(is.numeric(to) && length(to) == 2) to <- revgeocode(to)
  stopifnot(is.character(to))
  from_to_df <- data.frame(from = from, to = to, stringsAsFactors = FALSE)
  mode <- match.arg(mode)
  output <- match.arg(output)
  stopifnot(is.logical(messaging))


  getdists <- function(df){

  	# format basic url
    origins <- URLencode(df$from[1], reserved = TRUE)
    destinations <- URLencode(df$to, reserved = TRUE)
    posturl <- paste(
      fmteq(origins), fmteq(destinations, paste, collapse = "|"),
      fmteq(mode), fmteq(language),
      sep = "&"
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

    # join base url to posturl
    url_string <- paste0(
      sprintf("https://maps.googleapis.%s/maps/api/distancematrix/json?", ext),
      posturl
    )

    # inject
    if(inject != "") url_string <- paste(url_string, inject, sep = "&")

    # encode
    url_string <- URLencode( enc2utf8(url_string) )
    if(urlonly) return(url_string)

    # check if query is too long
    if(nchar(url_string) >= 2048){
      n <- nrow(df)
      half_df <- floor(n/2)
      return(
        rbind(
          getdists(df[half_df,]),
          getdists(df[(half_df+1):n,])
        )
      )
    }

    # check/update google query limit
    check_dist_query_limit(url_string, elems = nrow(df),
      override = override_limit, messaging = messaging)


    # distance lookup
    if(messaging) message("trying url ", url_string)
    connect <- url(url_string); on.exit(close(connect), add = TRUE)
    tree <- fromJSON(paste(readLines(connect), collapse = ""))
    check_google_for_error(tree)


    # message user
    message(paste0("Source : ", url_string))

    # label destinations - first check if all were found
    if(length(df$to) != length(tree$destination_addresses)){
      message("matching was not perfect, returning what was found.")
      names( tree$rows[[c(1,1)]] ) <- tree$destination_addresses
      output <<- "all"
      # stringdist::amatch(df$to, tree$destination_addresses, maxDist = 10)
    } else {
      names( tree$rows[[c(1,1)]] ) <- df$to
    }

    # return
    tree$rows[[c(1,1)]]
  }

  out <- dlply(from_to_df, "from", getdists)

  # return all
  if(output == "all") return(out)



  # format output
  out <-
    ldply(out, function(oneFromList){
      ldply(oneFromList, function(oneToList){
        data.frame(
          m = oneToList$distance$value,
  	      km = oneToList$distance$value/1000,
          miles = 0.0006214 * oneToList$distance$value,
          seconds = oneToList$duration$value,
          minutes = oneToList$duration$value / 60,
  	      hours = oneToList$duration$value / 3600
        )
      })
    })

  names(out) <- c("from", "to", names(out)[3:ncol(out)])

  # "simple" return
  suppressMessages(join(from_to_df, out))
}




check_dist_query_limit <- function(url_string, elems, override, messaging){

  .GoogleDistQueryCount <- NULL; rm(.GoogleDistQueryCount); # R CMD check trick

  if(exists(".GoogleDistQueryCount", .GlobalEnv)){

    .GoogleDistQueryCount <<- dplyr::filter(.GoogleDistQueryCount, time >= Sys.time() - 24*60*60)

    # limit per 24 hours
    dayQueriesUsed <- sum(.GoogleDistQueryCount$elements)
    if(dayQueriesUsed + elems > goog_day_limit()){
      message("query max exceeded, see ?mapdist.  current total = ", dayQueriesUsed)
      if(!override) return("stop")
    }

    # limit per second
    secondQueriesUsed <- with(.GoogleDistQueryCount, sum(elements[time >= Sys.time() - 1]))
    if(secondQueriesUsed + elems > goog_second_limit()){
      message(".", appendLF = FALSE)
      Sys.sleep(.2) # can do better
    }

    # append to .GoogleDistQueryCount
    .GoogleDistQueryCount <<- bind_rows(
      .GoogleDistQueryCount,
      data.frame(
        time = Sys.time(),  url = url_string,
        elements = elems, stringsAsFactors = FALSE
      )
    )


  } else {

    .GoogleDistQueryCount <<- data.frame(
      time = Sys.time(),  url = url_string,
      elements = elems, stringsAsFactors = FALSE
    )

  }
}



#' Check Google Maps Distance Matrix API query limit
#'
#' Check Google Maps Distance Matrix API query limit
#'
#' @return a data frame
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \url{http://code.google.com/apis/maps/documentation/distancematrix/}
#' @export
#' @examples
#' distQueryCheck()
distQueryCheck <- function(){

  .GoogleDistQueryCount <- NULL; rm(.GoogleDistQueryCount); # R CMD check trick

  if(exists(".GoogleDistQueryCount", .GlobalEnv)){

  	remaining <- goog_day_limit() - sum(
  	  dplyr::filter(.GoogleDistQueryCount, time >= Sys.time() - 24*60*60)$elements
  	)
    message(remaining, " mapdist queries remaining.")

  } else {

  	remaining <- goog_day_limit()
    message(remaining, " mapdist queries remaining.")

  }

  invisible(remaining)
}
