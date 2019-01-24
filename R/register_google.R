#' Register a Google API
#'
#' This page contains documentation for tools related to enabling Google
#' services in R. See the Details section of this file for background
#' information.
#'
#' As of mid-2018, the Google Maps Platform requires a registered API key. While
#' this alleviates previous burdens (e.g. query limits), it creates some
#' challenges as well. The most immediate challenge for most R users is that
#' ggmap functions that use Google's services no longer function out of the box,
#' since the user has to setup an account with Google, enable the relevant APIs,
#' and then tell R about the user's setup.
#'
#' To obtain an API key and enable services, go to
#' \url{https://cloud.google.com/maps-platform/}. This documentation shows you
#' how to input the requisite information (e.g. your API key) into R, and it
#' also shows you a few tools that can help you work with the credentialing.
#'
#' To tell ggmap about your API key, use the \code{register_google()} function,
#' e.g. \code{register_google(key = "mQkzTpiaLYjPqXQBotesgif3EfGL2dbrNVOrogg")}
#' (that's a fake key). This will set your API key for the current session, but
#' if you restart R, you'll need to do it again. You can set it permanently by
#' setting \code{write = TRUE}, see the examples. If you set it permanently it
#' will be stored in your .Renviron file, and that will be accessed by ggmap
#' persistently across sessions.
#'
#' Users should be aware that the API key, a string of jarbled
#' characters/numbers/symbols, is a PRIVATE key - it uniquely identifies and
#' authenticates you to Google's services. If anyone gets your API key, they can
#' use it to masquerade as you to Google and potentially use services that you
#' have enabled. Since Google requires a valid credit card to use its online
#' cloud services, this also means that anyone who obtains your key can
#' potentially make charges to your card in the form of Google services. So be
#' sure to not share your API key. To mitigate against users inadvertantly
#' sharing their keys, by default ggmap never displays a user's key in messages
#' displayed to the console.
#'
#' Users should also be aware that ggmap has no mechanism with which to
#' safeguard the private key once registered with R. That is to say, once you
#' register your API key, any function R will have access to it. As a
#' consequence, ggmap will not know if another function, potentially from a
#' compromised package, accesses the key and uploads it to a third party. For
#' this reason, when using ggmap we recommend a heightened sense of security and
#' self-awareness: only use trusted packages, do not save API keys in script
#' files, routinely cycle keys (regenerate new keys and retire old ones), etc.
#' Google offers features to help in securing your API key, including things
#' like limiting queries using that key to a particular IP address, as well as
#' guidance on security best practices. See
#' \url{https://cloud.google.com/docs/authentication/api-keys#securing_an_api_key}
#' for details.
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
#' @param write if TRUE, stores the secrets provided in the .Renviron file
#' @param ... a dumped formal argument to the generic print method
#' @return NULL
#' @name register_google
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \url{https://cloud.google.com/maps-platform/},
#'   \url{https://developers.google.com/maps/documentation/maps-static/get-api-key},
#'
#'   \url{https://developers.google.com/maps/documentation/maps-static/usage-and-billing}
#'
#' @examples
#'
#' # this sets your google map for this session
#' # register_google(key = "[your key]")
#'
#' # this sets your google map permanently
#' # register_google(key = "[your key]", write = TRUE)
#'
#' has_google_key()
#' google_key()
#' has_google_client()
#' has_google_signature()
#'
#' geocode("waco, texas", urlonly = TRUE)
#' ggmap_show_api_key()
#' geocode("waco, texas", urlonly = TRUE)
#' ggmap_hide_api_key()
#' geocode("waco, texas", urlonly = TRUE)
#'
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
register_google <- function (key, account_type, client, signature, second_limit, day_limit, write = FALSE) {


  # get current options
  options <- getOption("ggmap")


  # check for client/sig specs
  # if (!missing(client) &&  missing(signature) ) stop("if client is specified, signature must be also.")
  # if ( missing(client) && !missing(signature) ) stop("if signature is specified, client must be also.")
  if (!missing(client) && !missing(signature) ) {
    if (google_account() == "standard" && missing(account_type)) {
      stop("if providing client and signature, the account type must be premium.")
    }
  }


  # deal with api key
  if (!missing(key) && write) {

    # grab .Renviron file path
    environ_file <- file.path(Sys.getenv("HOME"), ".Renviron")

    # create .Renviron file if it does not exist
    if(!file.exists(file.path(Sys.getenv("HOME"), ".Renviron"))) {
      message(glue('Creating file {environ_file}'))
      file.create(environ_file)
    }

    # read in lines
    environ_lines <- readLines(environ_file)

    # if no key present, add; otherwise replace old one
    if (!any(str_detect(environ_lines, "GGMAP_GOOGLE_API_KEY="))) {

      message(glue('Adding key to {environ_file}'))
      environ_lines <- c(environ_lines, glue("GGMAP_GOOGLE_API_KEY={key}"))
      writeLines(environ_lines, environ_file)

    } else {

      key_line_index <- which(str_detect(environ_lines, "GGMAP_GOOGLE_API_KEY="))
      old_key <- str_extract(environ_lines[key_line_index], "(?<=GGMAP_GOOGLE_API_KEY=)\\w+")
      message(glue('Replacing old key ({old_key}) with new key in {environ_file}'))
      environ_lines[key_line_index] <- glue("GGMAP_GOOGLE_API_KEY={key}")
      writeLines(environ_lines, environ_file)

    }

    # set key in current session
    Sys.setenv("GGMAP_GOOGLE_API_KEY" = key)

  } else if (!missing(key) && !write) {

    # set key in current session
    Sys.setenv("GGMAP_GOOGLE_API_KEY" = key)

  }


  # deal with client
  if (!missing(client) && write) {

    # grab .Renviron file path
    environ_file <- file.path(Sys.getenv("HOME"), ".Renviron")

    # create .Renviron file if it does not exist
    if(!file.exists(file.path(Sys.getenv("HOME"), ".Renviron"))) {
      message(glue('Creating file {environ_file}'))
      file.create(environ_file)
    }

    # read in lines
    environ_lines <- readLines(environ_file)

    # if no client present, add; otherwise replace old one
    if (!any(str_detect(environ_lines, "GGMAP_GOOGLE_CLIENT="))) {

      message(glue('Adding client to {environ_file}'))
      environ_lines <- c(environ_lines, glue("GGMAP_GOOGLE_CLIENT={client}"))
      writeLines(environ_lines, environ_file)

    } else {

      client_line_index <- which(str_detect(environ_lines, "GGMAP_GOOGLE_CLIENT="))
      old_client <- str_extract(environ_lines[client_line_index], "(?<=GGMAP_GOOGLE_CLIENT=)\\w+")
      message(glue('Replacing old client ({old_client}) with new client in {environ_file}'))
      environ_lines[client_line_index] <- glue("GGMAP_GOOGLE_CLIENT={client}")
      writeLines(environ_lines, environ_file)

    }

    # set client in current session
    Sys.setenv("GGMAP_GOOGLE_CLIENT" = client)

  } else if (!missing(client) && !write) {

    # set client in current session
    Sys.setenv("GGMAP_GOOGLE_CLIENT" = client)

  }


  # deal with signature
  if (!missing(signature) && write) {

    # grab .Renviron file path
    environ_file <- file.path(Sys.getenv("HOME"), ".Renviron")

    # create .Renviron file if it does not exist
    if(!file.exists(file.path(Sys.getenv("HOME"), ".Renviron"))) {
      message(glue('Creating file {environ_file}'))
      file.create(environ_file)
    }

    # read in lines
    environ_lines <- readLines(environ_file)

    # if no signature present, add; otherwise replace old one
    if (!any(str_detect(environ_lines, "GGMAP_GOOGLE_SIGNATURE="))) {

      message(glue('Adding signature to {environ_file}'))
      environ_lines <- c(environ_lines, glue("GGMAP_GOOGLE_SIGNATURE={signature}"))
      writeLines(environ_lines, environ_file)

    } else {

      signature_line_index <- which(str_detect(environ_lines, "GGMAP_GOOGLE_SIGNATURE="))
      old_signature <- str_extract(environ_lines[signature_line_index], "(?<=GGMAP_GOOGLE_SIGNATURE=)\\w+")
      message(glue('Replacing old signature ({old_signature}) with new signature in {environ_file}'))
      environ_lines[signature_line_index] <- glue("GGMAP_GOOGLE_SIGNATURE={signature}")
      writeLines(environ_lines, environ_file)

    }

    # set signature in current session
    Sys.setenv("GGMAP_GOOGLE_SIGNATURE" = signature)

  } else if (!missing(signature) && !write) {

    # set signature in current session
    Sys.setenv("GGMAP_GOOGLE_SIGNATURE" = signature)

  }



  # construct new ones
  if(!missing(account_type)) options$google$account_type <- account_type
  if(!missing(day_limit)) options$google$day_limit <- day_limit
  if(!missing(second_limit)) options$google$second_limit <- second_limit

  # set premium defaults
  if (!missing(account_type) && account_type == "premium") {
    if(missing(day_limit)) options$google$day_limit <- 100000L
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

  cat("Key -",          if (!has_google_key()) "" else { if(showing_key()) google_key() else "xxx" }, "\n")
  cat("Account Type -", if(is.na(x[["account_type"]])) "" else x[["account_type"]], "\n")
  cat("Day Limit -",    if(is.na(x[["day_limit"]]))    "" else x[["day_limit"]],    "\n")
  cat("Second Limit -", if(is.na(x[["second_limit"]])) "" else x[["second_limit"]], "\n")
  cat("Client -",       if (!has_google_client())      "" else { if(showing_key()) google_client() else "xxx" }, "\n")
  cat("Signature -",    if (!has_google_signature())   "" else { if(showing_key()) google_signature() else "xxx" }, "\n")

}






#' @rdname register_google
#' @export
google_key <- function () {

  key <- Sys.getenv("GGMAP_GOOGLE_API_KEY")

  if (key == "") {
    return(NA_character_)
  } else {
    return(key)
  }

}


#' @rdname register_google
#' @export
has_google_key <- function () !is.na(google_key())





#' @rdname register_google
#' @export
has_google_account <- function () {

  if(is.null(getOption("ggmap"))) return(FALSE)

  !is.na(google_account())

}


#' @rdname register_google
#' @export
google_account <- function () getOption("ggmap")$google$account_type







#' @rdname register_google
#' @export
google_client <- function () {

  client <- Sys.getenv("GGMAP_GOOGLE_CLIENT")

  if (client == "") {
    return(NA_character_)
  } else {
    return(client)
  }

}


#' @rdname register_google
#' @export
has_google_client <- function () !is.na(google_client())





#' @rdname register_google
#' @export
google_signature <- function () {

  signature <- Sys.getenv("GGMAP_GOOGLE_SIGNATURE")

  if (signature == "") {
    return(NA_character_)
  } else {
    return(signature)
  }

}


#' @rdname register_google
#' @export
has_google_signature <- function () !is.na(google_signature())






#' @rdname register_google
#' @export
google_second_limit <- function () {

  # set to 50 if no key present (ggmap not loaded)
  if(!has_google_key()) return(50L)

  getOption("ggmap")$google$second_limit

}






#' @rdname register_google
#' @export
google_day_limit <- function () {

  # set to 2500 if no key present (ggmap not loaded)
  if(!has_google_key()) return(Inf)

  getOption("ggmap")$google$day_limit

}




