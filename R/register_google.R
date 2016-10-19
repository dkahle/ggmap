#' Register a Google API
#'
#' Many features of the Google web mapping services can be improved
#' by using standard or premium credentials, such as usage limits
#' and query rates. This function allows users to input this
#' information into R as a global option to be retained for the
#' entire session.
#'
#' @param key an api key
#' @param account_type \code{"standard"} or \code{"premium"}
#' @param client client code
#' @param signature signature code
#' @param second_limit query limit per second (default 50)
#' @param day_limit query limit per day (default 2500 for standard
#'   accounts, 100000 for premium accounts)
#' @return NULL
#' @name register_google
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso
#' \url{http://code.google.com/apis/maps/documentation/geocoding/},
#' \url{https://developers.google.com/maps/documentation/geocoding/usage-limits}
#' @examples
#'
#'
#' has_goog_key()
#' goog_key()
#' has_goog_client()
#' has_goog_signature()
#' register_google(key = "[your key here]")
#'




#' @rdname register_google
#' @export
register_google <- function (key, account_type, client, signature, second_limit, day_limit) {

  # get current options
  options <- getOption("ggmap")

  # check for client/sig specs
  if (!missing(client) &&  missing(signature) ) {
    stop("if client is specified, signature must be also.")
  }
  if ( missing(client) && !missing(signature) ) {
    stop("if signature is specified, client must be also.")
  }
  if (!missing(client) && !missing(signature) ) {
    if (goog_account() == "standard" && missing(account_type)) {
      stop("if providing client and signature, the account type must be premium.")
    }
  }

  # construct new ones
  if(!missing(key)) options$google$key <- key
  if(!missing(account_type)) options$google$account_type <- account_type
  if(!missing(day_limit)) options$google$day_limit <- day_limit
  if(!missing(second_limit)) options$google$second_limit <- second_limit
  if(!missing(client)) options$google$client <- client
  if(!missing(signature)) options$google$signature <- signature

  # # set premium defaults
  if (!missing(account_type) && account_type == "premium") {
    if(missing(day_limit)) options$google$day_limit <- 100000
  }

  # class
  class(options) <- "ggmap_credentials"

  # set new options
  options(ggmap = options)

  # return
  invisible(NULL)
}







#' @rdname register_google
#' @export
goog_key <- function () {

  getOption("ggmap")$google$key

}

#' @rdname register_google
#' @export
has_goog_key <- function () {

  if(is.null(getOption("ggmap"))) return(FALSE)

  !is.na(goog_key())

}







#' @rdname register_google
#' @export
has_goog_account <- function () {

  if(is.null(getOption("ggmap"))) return(FALSE)

  !is.na(goog_account())

}

#' @rdname register_google
#' @export
goog_account <- function () {

  getOption("ggmap")$google$account_type

}








#' @rdname register_google
#' @export
goog_client <- function () {

  getOption("ggmap")$google$client

}

#' @rdname register_google
#' @export
has_goog_client <- function () {

  if(is.null(getOption("ggmap"))) return(FALSE)

  !is.na(goog_client())

}








#' @rdname register_google
#' @export
goog_signature <- function () {

  getOption("ggmap")$google$signature

}

#' @rdname register_google
#' @export
has_goog_signature <- function () {

  if(is.null(getOption("ggmap"))) return(FALSE)

  !is.na(goog_signature())

}





#' @rdname register_google
#' @export
goog_second_limit <- function () {

  # set to 50 if no key present (ggmap not loaded)
  if(!has_goog_key()) return(50)

  getOption("ggmap")$google$second_limit

}



#' @rdname register_google
#' @export
goog_day_limit <- function () {

  # set to 2500 if no key present (ggmap not loaded)
  if(!has_goog_key()) return(2500)

  getOption("ggmap")$google$day_limit

}







#' @rdname register_google
#' @param tree a json tree from \code{\link{fromJSON}}
#' @export
check_google_for_error <- function (tree) {

  if (tree$status == "REQUEST_DENIED") {
    warning("REQUEST_DENIED : ", tree$error_message)
  }

}







