#' Register a Google API
#'
#' Many features of the Google web mapping services can be improved by using
#' standard or premium credentials, such as usage limits and query rates. This
#' function allows users to input this information into R as a global option to
#' be retained for the entire session.
#'
#' @param key an api key
#' @param account_type \code{"standard"} or \code{"premium"}
#' @param client client code
#' @param signature signature code
#' @param second_limit query limit per second (default 50)
#' @param day_limit query limit per day (default 2500 for standard accounts,
#'   100000 for premium accounts)
#' @param string a url string to be scrubbed. currently key, signature, and
#'   client keywords are scrubbed from the url and replace with the with
#'   argument
#' @param with a string to replace
#' @param x a google credentials class object
#' @param ... a dumped formal argument to the generic print method
#' @return NULL
#' @name register_google
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso
#' \url{https://developers.google.com/maps/documentation/maps-static/get-api-key},
#' \url{https://developers.google.com/maps/documentation/maps-static/usage-and-billing}
#' @examples
#'
#'
#' register_google(key = "[your key here]")
#'
#' has_google_key()
#' google_key()
#' has_google_client()
#' has_google_signature()
#' scrub_key("key=d_5iD")
#' scrub_key("key=d_5iD", "[your \\1]")
#' scrub_key("signature=d_5iD")
#' scrub_key("client=a_5sS&signature=d_5iD")
#'



#' @rdname register_google
#' @export
showing_key <- function () {
  getOption("ggmap")$display_api_key
}


#' @rdname register_google
#' @export
ggmap_show_api_key <- function () {
  set_ggmap_option("display_api_key" = TRUE)
  message("ggmap will now display PRIVATE api keys in the console.")
}


#' @rdname register_google
#' @export
ggmap_hide_api_key <- function () {
  set_ggmap_option("display_api_key" = FALSE)
  message("ggmap will now suppress private api keys in the console.")
}



#' @rdname register_google
#' @export
scrub_key <- function (string, with = "xxx") {
  str_replace_all(
    string,
    "(key|client|signature)=(\\w+)",
    str_c("\\1=", with)
  )
}



#' @rdname register_google
#' @export
register_google <- function (key, account_type, client, signature, second_limit, day_limit) {

  # get current options
  options <- getOption("ggmap")

  # check for client/sig specs
  if (!missing(client) &&  missing(signature) ) stop("if client is specified, signature must be also.")
  if ( missing(client) && !missing(signature) ) stop("if signature is specified, client must be also.")
  if (!missing(client) && !missing(signature) ) {
    if (google_account() == "standard" && missing(account_type)) {
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

  # set premium defaults
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
print.google_credentials <- function (x, ...) {

  cat("Key -", ifelse(is.na(x[["key"]]), '', x[["key"]]), "\n")
  cat("Account Type -", ifelse(is.na(x[["account_type"]]), '', x[["account_type"]]), "\n")
  cat("Day Limit -", ifelse(is.na(x[["day_limit"]]), '', x[["day_limit"]]), "\n")
  cat("Second Limit -", ifelse(is.na(x[["second_limit"]]), '', x[["second_limit"]]), "\n")
  cat("Client -", ifelse(is.na(x[["client"]]), '', x[["client"]]), "\n")
  cat("Signature -", ifelse(is.na(x[["signature"]]), '', x[["signature"]]), "\n")

}



#' @rdname register_google
#' @export
google_key <- function () getOption("ggmap")$google$key

#' @rdname register_google
#' @export
goog_key <- function () {
  .Deprecated("google_key()")
  google_key()
}





#' @rdname register_google
#' @export
has_google_key <- function () {

  if(is.null(getOption("ggmap"))) return(FALSE)

  !is.na(google_key())

}

#' @rdname register_google
#' @export
has_goog_key <- function() {
  .Deprecated("has_google_key()")
  has_google_key()
}





#' @rdname register_google
#' @export
has_google_account <- function () {

  if(is.null(getOption("ggmap"))) return(FALSE)

  !is.na(google_account())

}

#' @rdname register_google
#' @export
has_goog_account <- function() {
  .Deprecated("has_google_account()")
  has_google_key()
}





#' @rdname register_google
#' @export
google_account <- function () getOption("ggmap")$google$account_type

#' @rdname register_google
#' @export
goog_account <- function () {
  .Deprecated("google_account()")
  google_account()
}





#' @rdname register_google
#' @export
google_client <- function () getOption("ggmap")$google$client

#' @rdname register_google
#' @export
goog_client <- function () {
  .Deprecated("google_client()")
  google_client()
}





#' @rdname register_google
#' @export
has_google_client <- function () {

  if(is.null(getOption("ggmap"))) return(FALSE)

  !is.na(google_client())

}

#' @rdname register_google
#' @export
has_goog_client <- function () {
  .Deprecated("has_google_client")
  has_google_client()
}





#' @rdname register_google
#' @export
google_signature <- function () getOption("ggmap")$google$signature

#' @rdname register_google
#' @export
goog_signature <- function () {
  .Deprecated("google_signature()")
  google_signature()
}





#' @rdname register_google
#' @export
has_google_signature <- function () {

  if(is.null(getOption("ggmap"))) return(FALSE)

  !is.na(google_signature())

}

#' @rdname register_google
#' @export
has_goog_signature <- function () {
  .Deprecated("has_google_signature()")
  has_google_signature()
}





#' @rdname register_google
#' @export
google_second_limit <- function () {

  # set to 50 if no key present (ggmap not loaded)
  if(!has_google_key()) return(50L)

  getOption("ggmap")$google$second_limit

}

#' @rdname register_google
#' @export
goog_second_limit <- function () {
  .Deprecated("google_second_limit()")
  google_second_limit()
}





#' @rdname register_google
#' @export
google_day_limit <- function () {

  # set to 2500 if no key present (ggmap not loaded)
  if(!has_google_key()) return(Inf)

  getOption("ggmap")$google$day_limit

}

#' @rdname register_google
#' @export
goog_day_limit <- function () {
  .Deprecated("google_day_limit()")
  google_day_limit()
}





#' @rdname register_google
#' @param tree a json tree from \code{\link{fromJSON}}
#' @export
check_google_for_error <- function (tree) {

  if (tree$status == "REQUEST_DENIED") {
    warning("REQUEST_DENIED : ", tree$error_message)
  }

}







