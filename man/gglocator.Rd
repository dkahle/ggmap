\name{gglocator}
\alias{gglocator}
\title{Locator for ggplots.}
\usage{
  gglocator(n = 1, object = last_plot(), message = FALSE,
    xexpand = c(0.05, 0), yexpand = c(0.05, 0))
}
\arguments{
  \item{n}{number of points to locate.}

  \item{object}{plot to locate on}

  \item{message}{turn messaging from grid.ls on/off}

  \item{xexpand}{expand argument in scale_x_continuous}

  \item{yexpand}{expand argument in scale_y_continuous}
}
\value{
  a data frame with columns according to the x and y
  aesthetics
}
\description{
  Locator for ggplots.
}
\examples{
\dontrun{
df <- data.frame(xvar = 1:10, yvar = 1:10)
qplot(xvar, yvar, data = df) + annotate(geom = 'point', x = 3, y = 6)
gglocator(4)

qplot(xvar, yvar, data = df) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
gglocator(1, xexpand = c(0,0), yexpand = c(0,0))

}
}
\author{
  David Kahle \email{david.kahle@gmail.com} and Tyler
  Rinker with suggestions by Baptiste Auguie and DWin.
}

