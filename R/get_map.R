#' Grab a map.
#'
#' [get_map()] is a smart wrapper that queries the Google Maps,
#' OpenStreetMap, and Stadia Maps servers for a map.
#'
#' @param location an address, longitude/latitude pair (in that order), or
#'   left/bottom/right/top bounding box
#' @param zoom map zoom, an integer from 3 (continent) to 21 (building), default
#'   value 10 (city).  openstreetmaps limits a zoom of 18, and the limit on
#'   Stadia Maps depends on the maptype.  "auto" automatically determines the
#'   zoom for bounding box specifications, and is defaulted to 10 with
#'   center/zoom specifications.  maps of the whole world currently not
#'   supported.
#' @param scale scale argument of [get_googlemap()] or [get_openstreetmap()]
#' @param maptype character string providing map theme. options available are
#'   "terrain", "terrain-background", "satellite", "roadmap", and "hybrid"
#'   (Google Maps), "stamen_terrain", "stamen_toner", "stamen_toner_lite", "stamen_watercolor",
#'   "stamen_terrain_background", "stamen_toner_background",
#'   "stamen_terrain_lines", "stamen_terrain_labels",
#'   "stamen_toner_lines", "stamen_toner_labels"
#'   (Stadia Maps)
#' @param source Google Maps ("google"), OpenStreetMap ("osm"), Stadia Maps
#'   ("stadia")
#' @param force force new map (don't use archived version)
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param filename destination file for download (file extension added according
#'   to format). Default `NULL` means a random [tempfile()].
#' @param crop (Stadia and cloudmade maps) crop tiles to bounding box
#' @param color color ("color") or black-and-white ("bw")
#' @param language language for google maps
#' @param ... ...
#' @return a ggmap object (a classed raster object with a bounding box
#'   attribute)
#' @author David Kahle \email{david@@kahle.io}
#' @seealso [ggmap()]
#' @export
#' @examples
#'
#' \dontrun{ some requires Google API key, see ?register_google
#'
#' ## basic usage
#' ########################################
#'
#' # lon-lat vectors automatically use google:
#' (map <- get_map(c(-97.14667, 31.5493)))
#' str(map)
#' ggmap(map)
#'
#' # bounding boxes default to Stadia Maps
#' (map <- get_map(c(left = -97.1268, bottom = 31.536245, right = -97.099334, top = 31.559652)))
#' ggmap(map)
#'
#' # characters default to google
#' (map <- get_map("orlando, florida"))
#' ggmap(map)
#'
#'
#' ## basic usage
#' ########################################
#'
#' (map <- get_map(maptype = "roadmap"))
#' (map <- get_map(source = "osm"))
#' (map <- get_map(source = "stadia", maptype = "stamen_watercolor"))
#'
#' map <- get_map(location = "texas", zoom = 6, source = "stadia")
#' ggmap(map, fullpage = TRUE)
#'
#' }
get_map <- function(
  location = c(lon = -95.3632715, lat = 29.7632836),
  zoom = "auto",
  scale = "auto",
  maptype = c(GOOGLE_VALID_MAP_TYPES, STADIA_VALID_MAP_TYPES),
  source = c("google","osm","stadia"),
  force = ifelse(source == "google", TRUE, FALSE),
  messaging = FALSE,
  urlonly = FALSE,
  filename = NULL,
  crop = TRUE,
  color = c("color","bw"),
  language = "en-EN",
  ...
){

  # deprecated syntax
  args <- as.list(match.call(expand.dots = TRUE)[-1])
  if("verbose" %in% names(args)){
    .Deprecated(msg = "verbose argument deprecated, use messaging.")
    messaging <- eval(args$verbose)
  }

  if("center" %in% names(args)){
  	.Deprecated(msg = "center argument deprecated, use location.")
    location <- eval(args$center)
  }


  # preliminary argument checking
  source <- match.arg(source)
  color <- match.arg(color)
  if(missing(maptype)){
    if(source == "stadia"){
      maptype <- "stamen_terrain"
    }
    if(source != "cloudmade"){
      maptype <- "terrain"
    } else {
      maptype <- 1
    }
  }
  if(source == "stadia"){
    if(!(maptype %in% STADIA_VALID_MAP_TYPES)) {
      cli::cli_abort("Invalid Stadia {.arg maptype}, see {.fn get_stadiamap}.")
    }
  }
  if(source == "google" & !(
    maptype %in% GOOGLE_VALID_MAP_TYPES
  )){
    cli::cli_alert_warning("{.arg maptype = \"{maptype}\"} is only available with {.arg source = \"stadia\"}; resetting source.")
    source <- "stadia"
  }


  # location formatting
  location_stop <- TRUE

  if(is.character(location) && length(location) == 1){ # address
    location_type <- "address"
    location_stop <- FALSE
  }

  if(is.data.frame(location) && ncol(location) == 2){
    location <- colMeans(location) # hits the next one
  }

  if(is.numeric(location) && length(location) == 2){ # center/zoom
    location_type <- "lonlat"
    location_stop <- FALSE
    if(!is.null(names(location))){
      loc_names <- names(location)
      if(all(loc_names == c("long","lat"))){
        names(location) <- c("lon", "lat")
      } else if(all(loc_names == c("lat","lon"))){
        cli::cli_alert_info("Note : locations should be specified in the lon/lat format, not lat/lon.")
      	location <- location[c("lon","lat")]
      } else if(all(loc_names == c("lat","long"))){
        cli::cli_alert_info("Note : locations should be specified in the lon/lat format, not lat/lon.")
      	location <- location[c("long","lat")]
        names(location) <- c("lon", "lat")
      }
    } else { # is missing name the elements lon/lat
      names(location) <- c("lon","lat")
    }
  }

  if(is.numeric(location) && length(location) == 4){ # bbox
    location_type <- "bbox"
    location_stop <- FALSE

    # check bounding box
    if(length(names(location)) > 0){
      if(!all(names(location) %in% c("left","bottom","right","top"))){
        cli::cli_abort('Bounding boxes should have names {.code "left"}, {.code "bottom"}, {.code "right"}, {.code "top"}).')
      }
      location <- location[c("left","bottom","right","top")]
    } else {
      names(location) <- c("left","bottom","right","top")
    }
  }

  if(location_stop){ # if not one of the above, error
    cli::cli_abort("{.arg location} improperly specified, see {.fn ggmap::get_map}.")
  }


  # compute zoom when zoom = "auto"
  if(zoom == "auto" && location_type == "bbox"){
  	if(zoom == "auto"){
      lon_range <- location[c("left","right")]
      lat_range <- location[c("bottom","top")]

      # compute zoom
      if(missing(zoom)){
        lonlength <- diff(lon_range)
        latlength <- diff(lat_range)
        zoomlon <- ceiling( log2( 360*2 / lonlength) )
        zoomlat <- ceiling( log2( 180*2 / latlength) )
        zoom <- max(zoomlon, zoomlat)
      }
    }
  } else if(zoom == "auto" && location_type != "bbox"){
    zoom = 10
  }


  # compute scale when scale = "auto" (only for google/osm)
  if(scale == "auto"){
  	if(source == "google") scale <- 2
  	if(source == "osm") scale <- OSM_scale_lookup(zoom)
  }



  # Google Maps
  if(source == "google"){

  	# if bounding box given
    if(location_type == "bbox"){
      cli::cli_alert_warning("Bounding box given to Google - spatial extent only approximate.")

      # computer center
      user_bbox <- location
      location <- c(
        lon = mean(location[c("left","right")]),
        lat = mean(location[c("bottom","top")])
      )
    }

  	# get map
    map <- get_googlemap(center = location, zoom = zoom, maptype = maptype,
        scale = scale, messaging = messaging, urlonly = urlonly, force = force,
        filename = filename, color = color, language = language)

    # crop when bounding box is provided
    if(FALSE){
    bb <- attr(map, "bb")
    mbbox <- c(left = bb$ll.lon, bottom = bb$ll.lat, right = bb$ur.lon, top = bb$ur.lat)
    size <- dim(map)
    if(location_type == "bbox"){
      slon <- seq(mbbox["left"], mbbox["right"], length.out = size[1])
      slat <- seq(mbbox["top"], mbbox["bottom"], length.out = size[2])

      keep_x_ndcs <- which(user_bbox["left"] <= slon & slon <= user_bbox["right"])
      keep_y_ndcs <- which(user_bbox["bottom"] <= slat & slat <= user_bbox["top"])

      map <- map[keep_y_ndcs, keep_x_ndcs]
      class(map) <- c("ggmap","raster")
      attr(map, "bb") <- data.frame(
        ll.lat = user_bbox["bottom"], ll.lon = user_bbox["left"],
        ur.lat = user_bbox["top"], ur.lon = user_bbox["right"]
      )
    }
    }

    # return map
    return(map)
  }



  # OpenStreetMap
  if(source == "osm"){

  	if(location_type != "bbox"){
  	  # get bounding box
      gm <- get_googlemap(center = location, zoom = zoom, filename = filename)
      location <- as.numeric(attr(gm, "bb"))[c(2,1,4,3)]
    }

  	# get map/return
    return(
      get_openstreetmap(bbox = location, scale = scale,
        messaging = messaging, urlonly = urlonly, filename = filename,
        color = color)
    )
  }



  # Stadia Maps
  if(source == "stadia"){
  	if(location_type != "bbox"){
  	  # get bounding box
      gm <- get_googlemap(center = location, zoom = zoom, filename = filename)
      location <- as.numeric(attr(gm, "bb"))[c(2,1,4,3)]
    }

  	# get map/return
    return(
      get_stadiamap(bbox = location, zoom = zoom, maptype = maptype, crop = crop,
        messaging = messaging, urlonly = urlonly, filename = filename, force = force,
        color = color)
    )
  }



}
