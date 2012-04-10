#' Compute a bounding box
#'
#' Compute a bounding box for a given longitude / latitude collection.
#' 
#' @param lon longitude
#' @param lat latitude
#' @param data (optional) a data frame containing lon and lat as variables
#' @param f number specifying the fraction by which the range should be extended
#' @export
#' @examples
#' \donttest{
#'	
#' make_bbox(lon, lat, data = crime)
#'
#' (lon <- sample(crime$lon, 10))
#' (lat <- sample(crime$lat, 10))
#' make_bbox(lon, lat)
#' make_bbox(lon, lat, f = .10) # bigger box
#'
#' }
#'
make_bbox <- function(lon, lat, data, f = 0.05){
	
  if(!missing(data)){
    lon <- data[,deparse(substitute(lon))]
    lat <- data[,deparse(substitute(lat))]
  }
  
  lon_range <- extendrange(range(lon, na.rm = TRUE), f = f)  
  lat_range <- extendrange(range(lat, na.rm = TRUE), f = f)      
  c(left = lon_range[1], bottom = lat_range[1],
    right = lon_range[2], top = lat_range[2]) 
}     