\name{make_file_drawer}
\alias{make_file_drawer}
\title{Create a file cabinet for ggmap images}
\usage{
make_file_drawer(where = getwd(), drawerName = "ggmapFileDrawer")
}
\arguments{
  \item{where}{where the directory should be made}

  \item{drawerName}{the name of the directory}
}
\value{
TRUE if directory created; FALSE if not
}
\description{
Create a file cabinet for ggmap images
}
\details{
Changing the arguments is not recommended.
}
\examples{
\dontrun{

list.files()
file_drawer_found() # is the file drawer present?
make_file_drawer()
file_drawer_found()
list.files()
list.files('ggmapFileDrawer')
get_database()

}
}
\author{
David Kahle \email{david.kahle@gmail.com}.
}

