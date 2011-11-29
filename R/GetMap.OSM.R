#' Grab an OSM map.
#'
#' This is virtually identical to the function \code{\link{GetMap.OSM}} in the RgoogleMaps package.
#' 
#' @param lonR longitude range
#' @param latR latitude range
#' @param scale see \code{\link{GetMap.OSM}}
#' @param destfile see \code{\link{GetMap.OSM}}
#' @param format see \code{\link{GetMap.OSM}}
#' @param RETURNIMAGE see \code{\link{GetMap.OSM}}
#' @param GRAYSCALE see \code{\link{GetMap.OSM}}
#' @param NEWMAP see \code{\link{GetMap.OSM}}
#' @param verbose see \code{\link{GetMap.OSM}}
#' @param ... see \code{\link{GetMap.OSM}}
#' @return see \code{\link{GetMap.OSM}}
#' @author Markus Loecher, Sense Networks \email{markus@@sensenetworks.com}
#' @seealso \code{\link{GetMap.OSM}} in package RgoogleMaps
#'
GetMap.OSM <- function (lonR = c(-74.02132, -73.98622), latR = c(40.69983, 40.72595), 
  scale = 20000, destfile = "MyTile.png", format = "png", RETURNIMAGE = TRUE, 
  GRAYSCALE = FALSE, NEWMAP = TRUE, verbose = 1, ...
){
  options(scipen = 12)
  OSMbbox <- paste(lonR[1], latR[1], lonR[2], latR[2], sep = ",")
  OSMurl  <- "http://tile.openstreetmap.org/cgi-bin/export?"
  url     <- paste(OSMurl, "bbox=", OSMbbox, "&scale=", scale, "&format=", format, sep = "")
  if (verbose) print(url)
  if (NEWMAP) ret <- download.file(url, destfile, mode = "wb", quiet = !verbose)
  BBOX <- list(ll = c(latR[1], lonR[1]), ur = c(latR[2], lonR[2]))
  MetaInfo <- list(
    lat.center = mean(latR), 
    lon.center = mean(lonR), 
    zoom = NULL, url = "OSM", BBOX = BBOX, scale = scale
  )
  save(MetaInfo, file = paste(destfile, "rda", sep = "."))
  if (RETURNIMAGE) {
    myMap <- ReadMapTile(destfile)
    if (GRAYSCALE) 
    myMap$myTile <- RGB2GRAY(myMap$myTile)
    invisible(myMap)
  }
}  