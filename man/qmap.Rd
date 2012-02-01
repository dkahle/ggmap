\name{qmap}
\alias{qmap}
\title{Quick map plot}
\usage{
  qmap(...)
}
\arguments{
  \item{...}{stuff to pass to \code{\link{ggmapplot}} and
  \code{\link{ggmap}}.}
}
\value{
  a ggplot object
}
\description{
  qmap is a wrapper for \code{\link{ggmapplot}} and
  \code{\link{ggmap}}.
}
\examples{
\dontrun{
qmap(location = 'waco')
qmap(location = 'waco', scale = 17)
qmap(location = 'waco', scale = 17, fullpage = TRUE, verbose = TRUE)
}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}
\seealso{
  \code{\link{ggmapplot}} and \code{\link{ggmap}}.
}

