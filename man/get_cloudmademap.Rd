\name{get_cloudmademap}
\alias{get_cloudmademap}
\title{Get a CloudMade map}
\usage{
  get_cloudmademap(bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344),
    zoom = 10, api_key, maptype = 1, highres = TRUE,
    crop = TRUE, messaging = FALSE, urlonly = FALSE,
    filename = "ggmapTemp", color = c("color", "bw"),
    checkargs = TRUE)
}
\arguments{
  \item{bbox}{a bounding box in the format c(lowerleftlon,
  lowerleftlat, upperrightlon, upperrightlat).}

  \item{zoom}{a zoom level}

  \item{api_key}{character string containing cloud made api
  key, see details}

  \item{maptype}{an integer of what cloud made calls style,
  see details}

  \item{highres}{double resolution}

  \item{crop}{crop raw map tiles to specified bounding box}

  \item{messaging}{turn messaging on/off}

  \item{urlonly}{return url only}

  \item{filename}{destination file for download (file
  extension added according to format)}

  \item{color}{color or black-and-white}

  \item{checkargs}{check arguments}
}
\value{
  a map image as a 2d-array of colors as hexadecimal
  strings representing pixel fill values.
}
\description{
  get_cloudmademap accesses a tile server for Stamen Maps
  and downloads/stiches map tiles/formats a map image.
}
\details{
  accesses cloud made maps.  this function requires an api
  which can be obtained for free from
  \url{http://cloudmade.com/user/show}. thousands of
  maptypes ("styles"), including create-your-own options,
  are available from \url{http://maps.cloudmade.com/editor}
}
\examples{
\dontrun{

# in what follows, enter your own api key
api_key <- '<your api key here>'

map <- get_cloudmademap(api_key = api_key)
ggmap(map)

map <- get_cloudmademap(maptype = 997, api_key = api_key)
ggmap(map)

map <- get_cloudmademap(maptype = 31643, api_key = api_key)
ggmap(map)

map <- get_cloudmademap(maptype = 31408, api_key = api_key)
ggmap(map)

map <- get_cloudmademap(maptype = 15434, api_key = api_key)
ggmap(map)

map <- get_cloudmademap(maptype = 9203, api_key = api_key)
ggmap(map)

map <- get_cloudmademap(maptype = 53428, api_key = api_key)
ggmap(map)

map <- get_cloudmademap(maptype = 15153, api_key = api_key)
ggmap(map)

map <- get_cloudmademap(maptype = 7877, api_key = api_key)
ggmap(map)





}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \url{http://maps.cloudmade.com/}, \code{\link{ggmap}}
}

