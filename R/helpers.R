`%notin%` <- function(elem, set){
  !(elem %in% set)
}


# key <- "aBc"
# fmteq(key)
# fmteq(key, tolower)
# fmteq(key, toupper)
fmteq <- function (x, f = function(.) ., ...) {
  paste0(deparse(substitute(x)), "=", f(x, ...))
}
