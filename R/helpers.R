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





source_url_msg <- function(msg, strip_http = FALSE) {
  if (length(msg) > 1) return(invisible(sapply(msg, source_url_msg, strip_http = strip_http)))
  if (strip_http) msg <- gsub("https?://", "", msg)
  cli::cli_alert_info("{.url {msg}}")
}
# source_url_msg("https://www.kahle.io")
# source_url_msg("https://www.kahle.io", strip_http = FALSE)
# source_url_msg(c("https://www.kahle.io", "https://www.baylor.edu"))
# source_url_msg(c("https://www.kahle.io", "https://www.baylor.edu"), strip_http = FALSE)
