#' Compute map distances using Google
#'
#' Compute map distances using Google Maps. Note that in most cases by using this function you are agreeing to the Google Maps API Terms of Service at https://developers.google.com/maps/terms.
#'
#' @param from name of origin addresses in a data frame (vector accepted)
#' @param to name of destination addresses in a data frame (vector accepted)
#' @param output amount of output
#' @param mode driving, bicycling, or walking
#' @param messaging turn messaging on/off
#' @param sensor whether or not the geocoding request comes from a device with a location sensor
#' @param language language
#' @param override_limit override the current query count (.GoogleDistQueryCount)
#' @return a data frame (output="simple") or all of the geocoded information (output="all")
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @details if parameters from and to are specified as geographic coordinates, they are reverse geocoded with revgeocode.  note that the google maps api limits to 2500 element queries a day.
#' @seealso \url{http://code.google.com/apis/maps/documentation/distancematrix/}
#' @export
#' @examples
#'
#' mapdist("waco, texas", "houston, texas")
#'
#' from <- c("houston, texas", "dallas")
#' to <- "waco, texas"
#' mapdist(from, to)
#' mapdist(from, to, mode = "bicycling")
#' mapdist(from, to, mode = "walking")
#'
#' from <- c("houston", "houston", "dallas")
#' to <- c("waco, texas", "san antonio", "houston")
#' mapdist(from, to)
#'
#' mapdist("the white house", "washington monument", mode = "walking")
#'
#' # geographic coordinates are accepted as well
#' (wh <- as.numeric(geocode("the white house", source = "google")))
#' (wm <- as.numeric(geocode("washington monument", source = "google")))
#' mapdist(wh, wm, mode = "walking")
#' mapdist("the white house", wm, mode = "walking")
#' distQueryCheck()
#'
#'
mapdist <- function(from, to, mode = c("driving","walking","bicycling"),
  output = c("simple","all"), messaging = FALSE, sensor = FALSE,
  language = "en-EN", override_limit = FALSE)
{

  message("by using this function you are agreeing to the terms at :")
  message("http://code.google.com/apis/maps/documentation/distancematrix/\n")

  # check parameters
  if(is.numeric(from) && length(from) == 2) from <- revgeocode(from)
  stopifnot(is.character(from))
  if(is.numeric(to) && length(to) == 2) to <- revgeocode(to)
  stopifnot(is.character(to))
  from_to_df <- data.frame(from = from, to = to, stringsAsFactors = FALSE)
  origins <- from_to_df$from
  destinations <- from_to_df$to # this ensures # from = # to
  mode <- match.arg(mode)
  output <- match.arg(output)
  stopifnot(is.logical(messaging))
  stopifnot(is.logical(sensor))


  getdists <- function(df){

  	# format url
    origin <- df$from[1]
    origin <- gsub(",", "", origin)
    origin <- gsub(" ", "+", origin)
    origin <- paste("origins=", origin, sep = "")
    destinations <- df$to
    destinations <- gsub(",", "", destinations)
    destinations <- gsub(" ", "+", destinations)
    destinations <- paste("destinations=", paste(destinations, collapse = "|"), sep = "")
    mode4url <- paste("mode=", mode, sep = "")
    lang4url <- paste("language=", language, sep = "")
    sensor4url <- paste("sensor=", tolower(as.character(sensor)), sep = "")
    posturl <- paste(origin, destinations, mode4url, sensor4url, sep = "&")
    url_string <- paste("http://maps.googleapis.com/maps/api/distancematrix/json?",
      posturl, sep = "")
    url_string <- URLencode(url_string)


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
    connect <- url(url_string)
    tree <- fromJSON(paste(readLines(connect), collapse = ""))
    close(connect)

    # message user
    message(paste0("Information from URL : ", url_string))

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

    .GoogleDistQueryCount <<-
      subset(.GoogleDistQueryCount, time >= Sys.time() - 24*60*60)

    # 2500 per 24 hours
    if(sum(.GoogleDistQueryCount$elements) + elems > 2500){
      message("query max exceeded, see ?mapdist.  current total = ",
        sum(.GoogleDistQueryCount$elements))
      if(!override) stop("google query limit exceeded.", call. = FALSE)
    }

    # 100 per 10 seconds
    if(with(.GoogleDistQueryCount,
      sum(elements[time >= Sys.time() - 10]) + elems > 100
    )){
      if(messaging) message("waiting 10 seconds for another 100 queries...", appendLF=F)
      Sys.sleep(10) # can do better
      if(messaging) message(" done")
    }

    # append to .GoogleDistQueryCount
    .GoogleDistQueryCount <<- rbind(.GoogleDistQueryCount,
      data.frame(time = Sys.time(),  url = url_string,
        elements = elems, stringsAsFactors = FALSE)
    )


  } else {

    .GoogleDistQueryCount <<-
      data.frame(time = Sys.time(),  url = url_string,
        elements = elems, stringsAsFactors = FALSE)

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
  	remaining <- 2500-sum(
  	  subset(.GoogleDistQueryCount, time >= Sys.time() - 24*60*60)$elements
  	  )
    message(remaining, " distance queries remaining.")
  } else {
  	remaining <- 2500
    message(remaining, " distance queries remaining.")
  }
  invisible(remaining)
}
