`%notin%` <- function (elem, set) !(elem %in% set)

is.wholenumber <- function (x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol

# key <- "aBc"
# fmteq(key)
# fmteq(key, tolower)
# fmteq(key, toupper)
fmteq <- function (x, f = function(.) ., ...) {
  paste0(deparse(substitute(x)), "=", f(x, ...))
}



#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`




