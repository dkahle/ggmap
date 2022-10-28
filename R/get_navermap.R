#' Get a Naver Map
#'
#' This is (at least) temporarily unavailable as the Naver API changed.
#'
#' [get_navermap()] accesses the Naver Static Maps API version 1.1 to
#' download a static map. Note that in most cases by using this function you are
#' agreeing to the Naver Maps API Terms of Service.
#'
#' @param center the center of the map.  this can be longitude/latitude numeric
#'   vector.
#' @param zoom map zoom, an integer from 1 to 14 (building), default value 10
#' @param size rectangular dimensions of map in pixels - horizontal x vertical -
#'   with a max of c(640, 640).
#' @param format character string providing image format - png, jpeg(jpg)
#'   formats available in various flavors
#' @param crs Coordinate system, this currently supports EPSG:4326
#' @param baselayer base layer, this can be either "default", "satellite".
#' @param color color or black-and-white
#' @param overlayers overlay layers, this can be "anno_satellite","bicycle",
#'   "roadview", "traffic".
#' @param markers data.frame with first column longitude, second column
#'   latitude, for which naver markers should be embedded in the map image, or
#'   character string to be passed directly to api
#' @param key key code from naver api center
#' @param uri registered host url
#' @param filename destination file for download (file extension added according
#'   to format). Default `NULL` means a random [tempfile()].
#' @param messaging turn messaging on/off
#' @param urlonly return url only
#' @param force if the map is on file, should a new map be looked up?
#' @param where where should the file drawer be located (without terminating
#'   "/")
#' @param archiving use archived maps.  note: by changing to TRUE you agree to
#'   abide by any of the rules governing caching naver maps
#' @param ... ...
#' @author Heewon Jeon \email{madjakarta@@gmail.com}
#' @seealso [ggmap()]
#' @export
get_navermap <- function(
  center = c(lon = 126.9849208, lat = 37.5664519), zoom = 4,
  size = c(640,640), format = c("png", "jpeg", "jpg"),
  crs = c("EPSG:4326", "NHN:2048", "NHN:128", "EPSG:4258", "EPSG:4162", "EPSG:2096", "EPSG:2097", "EPSG:2098", "EPSG:900913"),
  baselayer = c("default", "satellite"), color = c("color","bw"),
  overlayers = c("anno_satellite", "bicycle", "roadview", "traffic"),
  markers, key, uri, filename = NULL, messaging = FALSE, urlonly = FALSE,
  force = FALSE, where = tempdir(), archiving = TRUE, ...
){

  .Defunct("Naver is at least temporarily not supported, as its API has changed.")

}


