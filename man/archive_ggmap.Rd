\name{archive_ggmap}
\alias{archive_ggmap}
\title{Store a ggmap object}
\usage{
  archive_ggmap(ggmap, url,
    file = paste0(timeStamp(), ".rds"),
    where = paste(getwd(), "ggmapFileDrawer", sep = "/"))
}
\arguments{
  \item{ggmap}{a ggmap object}

  \item{url}{url that map came from}

  \item{file}{file name (with extension .rds)}

  \item{where}{the ggmap file drawer}
}
\value{
  a data frame
}
\description{
  Store a ggmap object
}
\examples{
\dontrun{


list.files(recursive = TRUE)
map <- get_map()
url <- 'http://maps.googleapis.com/maps/api/staticmap?center=houston&zoom=10&size=\%20640x640&scale=\%202&maptype=terrain&sensor=false'
str(map)

list.files(recursive = TRUE)
get_database() # no database
archive_ggmap(map, url)
get_database()
list.files(recursive = TRUE)


}
}
\author{
  David Kahle \email{david.kahle@gmail.com}.
}

