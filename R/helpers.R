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

# convert color tile (as cached in the drawer) to grayscale
tile_to_bw <- function(color_tile) {
  bw <- col2rgb(color_tile)
  bw <- gray((0.30 * bw[1, ] + 0.59 * bw[2, ] + 0.11 * bw[3, ]) / 255)
  attributes(bw) <- attributes(color_tile)
  bw
}

# HTTP User-Agent string. Can be used with curl_download().
ggmap_useragent <- function() {
  paste0("ggmap/", packageVersion("ggmap"),
         sprintf(" R/%s.%s (%s)",
                 R.version[["major"]],
                 R.version[["minor"]],
                 R.version[["platform"]]))
}
