#' ggmap Options
#'
#' ggmap stores options as a named list in R's global options, i.e.
#' \code{getOption("ggmap")}. It currently stores two such options, one for
#' Google credentialing and one to suppress private API information in the URLs
#' printed to the screen when web queries are placed. For both of those, see
#' \code{\link{register_google}}.
#'
#' @param ... a named listing of options to set
#' @return NULL
#' @name ggmap_options
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{register_google}}
#' @examples
#'
#' getOption("ggmap")
#'




#' @rdname ggmap_options
#' @export
set_ggmap_option <- function(...) {

  # if there is no ggmap option (package is being initialized)
  # create the list with the arguments and return
  if ("ggmap" %notin% names(options())) {
    options("ggmap" = list(...))
    return(invisible())
  }

  # otherwise, go through arguments sequentially and add/update
  # them in the list ggmap in options
  ggmap <- getOption("ggmap")
  arg_list <- lapply(as.list(match.call())[-1], eval, envir = parent.frame())
  for (k in seq_along(arg_list)) {
    if (names(arg_list)[k] %in% names(ggmap)) {
      ggmap[names(arg_list)[k]] <- arg_list[k]
    } else {
      ggmap <- c(ggmap, arg_list[k])
    }
  }

  # set new ggmap
  options("ggmap" = ggmap)

  # return
  invisible()
}






#' @rdname ggmap_options
#' @export
ggmap_credentials <- function () {

  .Deprecated('getOption("ggmap")')
  getOption("ggmap")

}





# #' @rdname ggmap_options
# #' @export
# print.ggmap_credentials <- function (x, ...) {
#
#   for (k in seq_along(x)) {
#     cat(str_to_title(names(x)[k]), "- \n")
#     for (i in 1:length(x[[k]])) {
#       cat("  ", names(x[[k]])[i], ": ")
#       val <- x[[k]][[i]]
#       cat(ifelse(is.na(val), "", val), "\n")
#     }
#   }
#
# }



