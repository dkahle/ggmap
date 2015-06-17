#' Calculate a zoom given a bounding box
#'
#' calc_zoom can calculate a zoom based on either (1) a data frame with
#' longitude and latitude variables, (2) a longitude range and latitude range,
#' or (3) a bounding box (bbox specifcation). The specification for (1) is identical to that of
#' most R functions, for (2) simply put in a longitude range into lon and a
#' latitude range into lat, and for (3) put the bounding box in for the lon
#' argument.
#'
#' @param lon longitude, see details
#' @param lat latitude, see details
#' @param data (optional) a data frame containing lon and lat as variables
#' @param adjust number to add to the calculated zoom
#' @param f argument to pass to make_bbox
#' @export
#' @seealso \code{\link{make_bbox}}, \code{\link{bb2bbox}}
#' @examples
#' # From data
#' calc_zoom(lon, lat, wind)
#'
#' # From range
#' lon_range <- extendrange( wind$lon )
#' lat_range <- extendrange( wind$lat )
#' calc_zoom(lon_range, lat_range)
#'
#' # From bounding box
#' box <- make_bbox(lon, lat, data = crime)
#' calc_zoom(box)
calc_zoom <- function(lon, lat, data, adjust = 0, f = .05){

  if(!missing(adjust)) stopifnot(is.integer(adjust))

  # compute lon_range and lat_range for all specifications
  if(missing(data)){  # either ranges or a box

  	if(missing(lat)){ # a box
  	  bbox <- lon
  	  errorString <-
  	    "if specifying a bounding box, the format should match that of make_bbox."
  	  if(length(bbox) != 4) stop(errorString, call. = FALSE)
  	  if(!all(names(bbox) == c("left", "bottom", "right", "top")))
  	    stop(errorString, call. = FALSE)
      lon_range <- bbox[c("left", "right")]
      lat_range <- bbox[c("bottom", "top")]

  	} else { # ranges
  	  if(length(lon) != 2 || length(lat) != 2 || !is.numeric(lon) || !is.numeric(lat))
  	    stop("if specifying ranges, they both must be of length 2 and numeric.")

      lon_range <- sort(lon)
      lat_range <- sort(lat)
  	}

  } else { # data argument is specified
    lon <- data[,deparse(substitute(lon))]
    lat <- data[,deparse(substitute(lat))]
    bbox <- make_bbox(lon, lat, f = f)
    lon_range <- bbox[c("left", "right")]
    lat_range <- bbox[c("bottom", "top")]
  }



  lonlength <- diff(lon_range)
  latlength <- diff(lat_range)
  zoomlon <- ceiling(log2(360 * 2/lonlength))
  zoomlat <- ceiling(log2(180 * 2/latlength))
  zoom <- max(zoomlon, zoomlat)

  zoom + adjust
}
