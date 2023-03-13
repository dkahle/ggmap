<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggmap)](https://cran.r-project.org/package=ggmap)
[![Travis build
status](https://travis-ci.org/dkahle/ggmap.svg?branch=master)](https://travis-ci.org/dkahle/ggmap)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/dkahle/ggmap?branch=master&svg=true)](https://ci.appveyor.com/project/dkahle/ggmap)
<!-- badges: end -->

<hr>

# ggmap

**ggmap** is an R package that makes it easy to retrieve raster map
tiles from popular online mapping services like [Google
Maps](https://developers.google.com/maps/documentation/maps-static?hl=en)
and [Stamen Maps](http://maps.stamen.com) and plot them using the
[**ggplot2**](https://github.com/tidyverse/ggplot2) framework:

``` r
library("ggmap")
#  Loading required package: ggplot2
#  ℹ Google's Terms of Service: ]8;;https://mapsplatform.google.com<https://mapsplatform.google.com>]8;;
#  ℹ Please cite ggmap if you use it! Use `citation("ggmap")` for details.

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% ggmap() 
#  ℹ Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
```

![](tools/README-maptypes-1.png)

Use `qmplot()` in the same way you’d use `qplot()`, but with a map
automatically added in the background:

``` r
library("dplyr")
#  
#  Attaching package: 'dplyr'
#  The following objects are masked from 'package:stats':
#  
#      filter, lag
#  The following objects are masked from 'package:base':
#  
#      intersect, setdiff, setequal, union
library("forcats")

# define helper
`%notin%` <- function(lhs, rhs) !(lhs %in% rhs)

# reduce crime to violent crimes in downtown houston
violent_crimes <- crime %>% 
  filter(
    offense %notin% c("auto theft", "theft", "burglary"),
    between(lon, -95.39681, -95.34188),
    between(lat, 29.73631, 29.78400)
  ) %>% 
  mutate(
    offense = fct_drop(offense),
    offense = fct_relevel(offense, c("robbery", "aggravated assault", "rape", "murder"))
  )

# use qmplot to make a scatterplot on a map
qmplot(lon, lat, data = violent_crimes, maptype = "toner-lite", color = I("red"))
#  ℹ Using `zoom = 14`
#  ℹ Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
```

![](tools/README-qmplot-1.png)

All the **ggplot2** geom’s are available. For example, you can make a
contour plot with `geom = "density2d"`:

``` r
qmplot(lon, lat, data = violent_crimes, maptype = "toner-lite", geom = "density2d", color = I("red"))
```

In fact, since **ggmap**’s built on top of **ggplot2**, all your usual
**ggplot2** stuff (geoms, polishing, etc.) will work, and there are some
unique graphing perks **ggmap** brings to the table, too.

``` r
robberies <- violent_crimes %>% filter(offense == "robbery")

library("ggdensity")
library("geomtextpath")

qmplot(lon, lat, data = violent_crimes, geom = "blank", 
  zoom = 14, maptype = "toner-background"
) +
  geom_hdr(aes(fill = stat(probs)), alpha = .3) +
  geom_labeldensity2d(aes(lon, lat, level = stat(probs)), stat = "hdr_lines") +
  scale_fill_viridis_d(option = "A") +
  theme(legend.position = "none")
#  ℹ Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#362: if (messaging) source_url_msg(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#377: response <- httr::GET(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#381: if (response$status_code != 200L) {
#      httr::message_for_status(response, glue("acquire tile /{maptype}/{zoom}/{x}/{y}.{filetype}"))
#      if (messaging) 
#          message("\n", appendLF = FALSE)
#      log_stamen_tile_download_fail(url)
#      tile <- matrix(rgb(1, 1, 1, 0), nrow = 256L, ncol = 256L)
#  } else {
#      tile <- httr::content(response)
#      tile <- aperm(tile, c(2, 1, 3))
#      if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#          "terrain-labels", "terrain-lines")) {
#          if (color == "color") {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#          else {
#              tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#                  x[3], x[4]))
#          }
#      }
#      else {
#          if (color == "color") {
#              tile <- apply(tile, 2, rgb)
#          }
#          else {
#              tiled <- dim(tile)
#              tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 
#                  2] + 0.11 * tile[, , 3])
#              dim(tile) <- tiled[1:2]
#          }
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#391: tile <- httr::content(response)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#392: tile <- aperm(tile, c(2, 1, 3))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#395: if (maptype %in% c("toner-hybrid", "toner-labels", "toner-lines", 
#      "terrain-labels", "terrain-lines")) {
#      if (color == "color") {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#      else {
#          tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], 
#              x[3], x[4]))
#      }
#  } else {
#      if (color == "color") {
#          tile <- apply(tile, 2, rgb)
#      }
#      else {
#          tiled <- dim(tile)
#          tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 
#              0.11 * tile[, , 3])
#          dim(tile) <- tiled[1:2]
#      }
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#405: if (color == "color") {
#      tile <- apply(tile, 2, rgb)
#  } else {
#      tiled <- dim(tile)
#      tile <- gray(0.3 * tile[, , 1] + 0.59 * tile[, , 2] + 0.11 * 
#          tile[, , 3])
#      dim(tile) <- tiled[1:2]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#406: tile <- apply(tile, 2, rgb)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#426: lonlat_upperleft <- XY2LonLat(x, y, zoom)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#427: lonlat_lowerright <- XY2LonLat(x, y, zoom, 255L, 255L)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#429: bbox <- c(left = lonlat_upperleft$lon, bottom = lonlat_lowerright$lat, 
#      right = lonlat_lowerright$lon, top = lonlat_upperleft$lat)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#436: bb <- tibble(ll.lat = unname(bbox["bottom"]), ll.lon = unname(bbox["left"]), 
#      ur.lat = unname(bbox["top"]), ur.lon = unname(bbox["right"]))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#445: class(tile) <- c("ggmap", "raster")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#446: attr(tile, "bb") <- bb
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#450: file_drawer_set(url, tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#454: tile
#  Warning in geom_labeldensity2d(aes(lon, lat, level = stat(probs)), stat =
#  "hdr_lines"): Ignoring unknown parameters: `contour`, `contour_var`, `h`, and
#  `adjust`
#  Warning in geom_labeldensity2d(aes(lon, lat, level = stat(probs)), stat =
#  "hdr_lines"): Ignoring unknown aesthetics: level
#  Warning: `stat(probs)` was deprecated in ggplot2 3.4.0.
#  ℹ Please use `after_stat(probs)` instead.
```

![](tools/README-styling-1.png)

Faceting works, too:

``` r
qmplot(lon, lat, data = violent_crimes, maptype = "toner-background", color = offense) + 
  facet_wrap(~ offense)
#  ℹ Using `zoom = 14`
#  ℹ Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
#  Called from: get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, 
#      messaging = messaging, https = https)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#342: if (missing(url)) {
#      stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#      stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#      stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#      if (maptype %in% c("watercolor")) 
#          filetype <- "jpg"
#      else filetype <- "png"
#      domain <- if (https) 
#          "https://stamen-tiles.a.ssl.fastly.net"
#      else "http://tile.stamen.com"
#      url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#      tile <- file_drawer_get(url)
#      if (!is.null(tile) && !force) 
#          return(tile)
#      if (messaging) 
#          source_url_msg(url)
#  } else {
#      url_pieces <- url %>% str_split("[/.]") %>% pluck(1L)
#      maptype <- url_pieces[6]
#      zoom <- url_pieces[7] %>% as.integer()
#      x <- url_pieces[8] %>% as.integer()
#      y <- url_pieces[9] %>% as.integer()
#      filetype <- url_pieces[10]
#  }
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#345: stopifnot(is.wholenumber(zoom) || !(zoom %in% 1:20))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#346: stopifnot(is.wholenumber(x) || !(0 <= x && x < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#347: stopifnot(is.wholenumber(y) || !(0 <= y && y < 2^zoom))
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: if (maptype %in% c("watercolor")) filetype <- "jpg" else filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#351: filetype <- "png"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: domain <- if (https) "https://stamen-tiles.a.ssl.fastly.net" else "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#352: [1] "http://tile.stamen.com"
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#353: url <- glue("{domain}/{maptype}/{zoom}/{x}/{y}.{filetype}")
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#357: tile <- file_drawer_get(url)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: if (!is.null(tile) && !force) return(tile)
#  debug at /Users/david_kahle/Dropbox/dev/ggmap/ggmap/R/get_stamenmap.R#358: return(tile)
```

![](tools/README-faceting-1.png)

## Google Maps

[Google Maps](https://cloud.google.com/maps-platform/terms/) can be used
just as easily. However, since Google Maps use a center/zoom
specification, their input is a bit different:

``` r
(map <- get_googlemap("waco texas", zoom = 12))
#  ℹ <]8;;https://maps.googleapis.com/maps/api/staticmap?center=waco%20texas&zoom=12&size=640x640&scale=2&maptype=terrain&key=xxxhttps://maps.googleapis.com/maps/api/staticmap?center=waco%20texas&zoom=12&size=640x640&scale=2&maptype=terrain&key=xxx]8;;>
#  ℹ <]8;;https://maps.googleapis.com/maps/api/geocode/json?address=waco+texas&key=xxxhttps://maps.googleapis.com/maps/api/geocode/json?address=waco+texas&key=xxx]8;;>
#  1280x1280 terrain map image from Google Maps; use `]8;;ide:help:ggmap::ggmapggmap::ggmap]8;;()` to plot it.
ggmap(map)
```

![](tools/README-google_maps-1.png)

Moreover, you can get various different styles of Google Maps with
**ggmap** (just like Stamen Maps):

``` r
get_googlemap("waco texas", zoom = 12, maptype = "satellite") %>% ggmap()
get_googlemap("waco texas", zoom = 12, maptype = "hybrid") %>% ggmap()
get_googlemap("waco texas", zoom = 12, maptype = "roadmap") %>% ggmap()
```

Google’s geocoding and reverse geocoding API’s are available through
`geocode()` and `revgeocode()`, respectively:

``` r
geocode("1301 S University Parks Dr, Waco, TX 76798")
#  ℹ <]8;;https://maps.googleapis.com/maps/api/geocode/json?address=1301+S+University+Parks+Dr,+Waco,+TX+76798&key=xxxhttps://maps.googleapis.com/maps/api/geocode/json?address=1301+S+University+Parks+Dr,+Waco,+TX+76798&key=xxx]8;;>
#  # A tibble: 1 × 2
#      lon   lat
#    <dbl> <dbl>
#  1 -97.1  31.6
revgeocode(c(lon = -97.1161, lat = 31.55098))
#  ℹ <]8;;https://maps.googleapis.com/maps/api/geocode/json?latlng=31.55098,-97.1161&key=xxxhttps://maps.googleapis.com/maps/api/geocode/json?latlng=31.55098,-97.1161&key=xxx]8;;>
#  Warning: Multiple addresses found, the first will be returned:
#  !   1301 S University Parks Dr, Waco, TX 76706, USA
#  !   55 Baylor Ave, Waco, TX 76706, USA
#  !   HV2M+9H Waco, TX, USA
#  !   Bear Trail, Waco, TX 76706, USA
#  !   Robinson, TX 76706, USA
#  !   Waco, TX, USA
#  !   McLennan County, TX, USA
#  !   Texas, USA
#  !   United States
#  [1] "1301 S University Parks Dr, Waco, TX 76706, USA"
```

*Note: `geocode()` uses Google’s Geocoding API to geocode addresses.
Please take care not to disclose sensitive information. [Rundle, Bader,
and Moody (2022)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8972108/)
have considered this issue and suggest various alternative options for
such data.*

There is also a `mutate_geocode()` that works similarly to
[**dplyr**](https://github.com/tidyverse/dplyr/)’s `mutate()` function:

``` r
tibble(address = c("white house", "", "waco texas")) %>% 
  mutate_geocode(address)
#  ℹ <]8;;https://maps.googleapis.com/maps/api/geocode/json?address=white+house&key=xxxhttps://maps.googleapis.com/maps/api/geocode/json?address=white+house&key=xxx]8;;>
#  ℹ <]8;;https://maps.googleapis.com/maps/api/geocode/json?address=waco+texas&key=xxxhttps://maps.googleapis.com/maps/api/geocode/json?address=waco+texas&key=xxx]8;;>
#  # A tibble: 3 × 3
#    address         lon   lat
#    <chr>         <dbl> <dbl>
#  1 "white house" -77.0  38.9
#  2 ""             NA    NA  
#  3 "waco texas"  -97.1  31.5
```

Treks use Google’s routing API to give you routes (`route()` and
`trek()` give slightly different results; the latter hugs roads):

``` r
trek_df <- trek("houson, texas", "waco, texas", structure = "route")
#  ℹ <]8;;https://maps.googleapis.com/maps/api/directions/json?origin=houson,+texas&destination=waco,+texas&key=xxx&mode=driving&alternatives=false&units=metrichttps://maps.googleapis.com/maps/api/directions/json?origin=houson,+texas&destination=waco,+texas&key=xxx&mode=driving&alternatives=false&units=metric]8;;>
qmap("college station, texas", zoom = 8) +
  geom_path(
    aes(x = lon, y = lat),  colour = "blue",
    size = 1.5, alpha = .5,
    data = trek_df, lineend = "round"
  )
#  ℹ <]8;;https://maps.googleapis.com/maps/api/staticmap?center=college%20station,%20texas&zoom=8&size=640x640&scale=2&maptype=terrain&language=en-EN&key=xxxhttps://maps.googleapis.com/maps/api/staticmap?center=college%20station,%20texas&zoom=8&size=640x640&scale=2&maptype=terrain&language=en-EN&key=xxx]8;;>
#  ℹ <]8;;https://maps.googleapis.com/maps/api/geocode/json?address=college+station,+texas&key=xxxhttps://maps.googleapis.com/maps/api/geocode/json?address=college+station,+texas&key=xxx]8;;>
#  Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#  ℹ Please use `linewidth` instead.
```

![](tools/README-route_trek-1.png)

(They also provide information on how long it takes to get from point A
to point B.)

Map distances, in both length and anticipated time, can be computed with
`mapdist()`). Moreover the function is vectorized:

``` r
mapdist(c("houston, texas", "dallas"), "waco, texas")
#  ℹ <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=dallas&destinations=waco,+texas&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=dallas&destinations=waco,+texas&key=xxx&mode=driving]8;;>
#  ℹ <]8;;https://maps.googleapis.com/maps/api/distancematrix/json?origins=houston,+texas&destinations=waco,+texas&key=xxx&mode=drivinghttps://maps.googleapis.com/maps/api/distancematrix/json?origins=houston,+texas&destinations=waco,+texas&key=xxx&mode=driving]8;;>
#  # A tibble: 2 × 9
#    from           to               m    km miles seconds minutes hours mode   
#    <chr>          <chr>        <int> <dbl> <dbl>   <int>   <dbl> <dbl> <chr>  
#  1 dallas         waco, texas 155586  156.  96.7    5336    88.9  1.48 driving
#  2 houston, texas waco, texas 295004  295. 183.    10311   172.   2.86 driving
```

## Google Maps API key

A few years ago Google has [changed its API
requirements](https://developers.google.com/maps/documentation/geocoding/usage-and-billing),
and **ggmap** users are now required to register with Google. From a
user’s perspective, there are essentially three ramifications of this:

1.  Users must register with Google. You can do this at
    <https://mapsplatform.google.com>. While it will require a valid
    credit card (sorry!), there seems to be a fair bit of free use
    before you incur charges, and even then the charges are modest for
    light use.

2.  Users must enable the APIs they intend to use. What may appear to
    **ggmap** users as one overarching “Google Maps” product, Google in
    fact has several services that it provides as geo-related solutions.
    For example, the [Maps Static
    API](https://developers.google.com/maps/documentation/maps-static/overview)
    provides map images, while the [Geocoding
    API](https://developers.google.com/maps/documentation/geocoding/overview)
    provides geocoding and reverse geocoding services. Apart from the
    relevant Terms of Service, generally **ggmap** users don’t need to
    think about the different services. For example, you just need to
    remember that `get_googlemap()` gets maps, `geocode()` geocodes
    (with Google, DSK is done), etc., and **ggmap** handles the queries
    for you. *However*, you do need to enable the APIs before you use
    them. You’ll only need to do that once, and then they’ll be ready
    for you to use. Enabling the APIs just means clicking a few radio
    buttons on the Google Maps Platform web interface listed above, so
    it’s easy.

3.  Inside R, after loading the new version of **ggmap**, you’ll need
    provide **ggmap** with your API key, a [hash
    value](https://en.wikipedia.org/wiki/Hash_function) (think string of
    jibberish) that authenticates you to Google’s servers. This can be
    done on a temporary basis with `register_google(key = "[your key]")`
    or permanently using
    `register_google(key = "[your key]", write = TRUE)` (note: this will
    overwrite your `~/.Renviron` file by replacing/adding the relevant
    line). If you use the former, know that you’ll need to re-do it
    every time you reset R.

Your API key is *private* and unique to you, so be careful not to share
it online, for example in a GitHub issue or saving it in a shared R
script file. If you share it inadvertantly, just get on Google’s website
and regenerate your key - this will retire the old one. Keeping your key
private is made a bit easier by **ggmap** scrubbing the key out of
queries by default, so when URLs are shown in your console, they’ll look
something like `key=xxx`. (Read the details section of the
`register_google()` documentation for a bit more info on this point.)

The new version of **ggmap** is now on CRAN soon, but you can install
the latest version, including an important bug fix in `mapdist()`, here
with:

``` r
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap")
```

## Installation

-   From CRAN: `install.packages("ggmap")`

-   From Github:

``` r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("dkahle/ggmap")
```
