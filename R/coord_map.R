# Replacement for ggplot2:::mproject when using "mercator" projection
mproject2 <- function(coord, x, y, orientation){
  npix <- 2 * pi
  xXyY <- suppressWarnings(LonLat2XY(x, y, zoom = 0,
                                     xpix = npix, ypix = npix))
  x2 <- (npix * xXyY$X + xXyY$x) - pi
  x2[is.nan(x2)] <- NA_real_
  y2 <- pi - (npix * xXyY$Y + xXyY$y)
  y2[is.nan(y2)] <- NA_real_
  list(x = x2, y = y2,
       range = c(range(x2, na.rm = TRUE), range(y2, na.rm = TRUE)),
       error = 0)
}

# Modified ggplot2::coord_map, replaces uses of ggplot2:::mproject
# with mproject2. Dependence on ggplot2 (version 2.2.0) internals is
# unfortunate.
coord_map2 <- function(){
  trans_fun <- get("f", environment(CoordMap$transform))
  trans_env <- new.env(parent = environment(trans_fun))
  assign("mproject", mproject2, trans_env)
  environment(trans_fun) <- trans_env
  train_fun <- get("f", environment(CoordMap$train))
  train_env <- new.env(parent = environment(train_fun))
  assign("mproject", mproject2, train_env)
  environment(train_fun) <- train_env
  ggproto(
    NULL, CoordMap, projection = "mercator", orientation = NULL,
    limits = list(x = NULL, y = NULL), params = list(),
    transform = trans_fun, train = train_fun
  )
}
