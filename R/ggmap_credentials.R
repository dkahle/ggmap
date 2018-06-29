#' ggmap Credentials
#'
#' Get ggmap's credentials; these are all Google credentials at this
#' point.
#'
#' @param x the ggmap credentials object; getOption("ggmap")
#' @param ... ... (dumped)
#' @return NULL
#' @name ggmap_credentials
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{register_google}}
#' @examples
#'
#' ggmap_credentials()
#'






#' @rdname ggmap_credentials
#' @export
ggmap_credentials <- function () {

  getOption("ggmap")

}





#' @rdname ggmap_credentials
#' @export
print.ggmap_credentials <- function (x, ...) {

  for (k in 1:length(x)) {
    cat(str_to_title(names(x)[k]), "- \n")
    for (i in 1:length(x[[k]])) {
      cat("  ", names(x[[k]])[i], ": ")
      val <- x[[k]][[i]]
      cat(ifelse(is.na(val), "", val), "\n")
    }
  }

}


