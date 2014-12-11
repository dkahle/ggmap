#' Fetch tile region.
#'
#' Given bounding box, find tiles that span region and stitch them together
#' into single raster.
#'
#' @keywords internal
#' @examples
#' fetch_region(c(-95.80204, -94.92313), c(29.38048, 30.14344), "stamen", "terrain")
fetch_region <- function(lon, lat, provider, ..., cache = TRUE, zoom = 10) {
  meta <- bbox_tiles(lon, lat, zoom = zoom)
  meta$url <- tile_urls(meta, provider, ...)
  tiles <- lapply(meta$url, fetch_tile, cache = cache)

  region <- stitch_tiles(meta, tiles)

  # Compute pixel coordinates
  x <- lon2x(range(lon), zoom)
  l <- x$x[1] + 1
  r <- diff(x$X) * 256 + x$x[2] + 1

  y <- lat2y(rev(range(lat)), zoom)
  b <- y$y[1] + 1
  t <- diff(y$Y) * 256 + y$y[2] + 1

  # Crop & save bounding box
  clipped <- region[b:t, l:r]
  class(clipped) <- c("tile", "raster")
  clipped
}

bbox_tiles <- function(lon, lat, zoom = 10) {
  lon <- range(lon, na.rm = TRUE)
  lat <- range(lat, na.rm = TRUE)

  br <- LonLat2XY(lon[1], lat[1], zoom)
  tl <- LonLat2XY(lon[2], lat[2], zoom)

  xs <- seq(br$X, tl$X)
  ys <- seq(br$Y, tl$Y)

  tiles <- expand.grid(y = ys, x = xs, KEEP.OUT.ATTRS = FALSE)
  tiles$z <- zoom
  tiles
}

tile_urls <- function(meta, provider, ...) {
  providers[[provider]]$tile_f(meta$x, meta$y, meta$z, ...)
}

fetch_tile <- function(url, cache = TRUE, quiet = FALSE) {
  tile <- file_drawer_get(url)
  if (!is.null(tile) && cache) return(tile)

  if (!quiet)
    message("Fetching ", url)
  r <- httr::GET(url)
  httr::stop_for_status(r)

  tile <- httr::content(r, "parsed")
  tile <- t(apply(tile, 2, rgb))
  attr(tile, "url") <- url
  class(tile) <- c("tile", "raster")
  if (cache) {
    file_drawer_set(url, tile)
  }
  tile
}

#' @export
print.tile <- function(x, ...) {
  old <- par(mar = c(0, 0, 0, 0))
  on.exit(par(old))

  plot.new()
  plot.window(c(0, ncol(x)), c(0, nrow(x)), asp = 1)
  rasterImage(x, 0, 0, ncol(x), nrow(x))
}

stitch_tiles <- function(meta, tiles) {
  stopifnot(length(tiles) == nrow(meta))

  w <- length(unique(meta$x))
  h <- length(unique(meta$y))

  out <- matrix(NA_character_, w * 256, h * 256)

  xs <- (match(meta$x, sort(unique(meta$x))) - 1) * 256
  ys <- (match(meta$y, sort(unique(meta$y))) - 1) * 256

  for (i in seq_along(tiles)) {
    out[xs[i]:(xs[i] + 255) + 1, ys[i]:(ys[i] + 255) + 1] <- tiles[[i]]
  }
  class(out) <- c("tile", "raster")

  dim(out) <- rev(dim(out))
  out
}
