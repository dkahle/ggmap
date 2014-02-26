\name{get_database}
\alias{get_database}
\title{Get the ggmap database}
\usage{
get_database(where = paste(getwd(), "ggmapFileDrawer", sep = "/"))
}
\arguments{
  \item{where}{where the directory should be made}
}
\value{
a data frame
}
\description{
Get the ggmap database
}
\details{
The ggmap database is a data frame with two columns : the
url which needs to be called for the map (or maptile) and
the image filename.
}
\examples{
\dontrun{

list.files()
file_drawer_found() # is the file drawer present?
make_file_drawer()
file_drawer_found()
list.files()
str(get_database())

}
}
\author{
David Kahle \email{david.kahle@gmail.com}.
}

