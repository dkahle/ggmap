\name{theme_nothing}
\alias{theme_nothing}
\title{Make a blank ggplot2 theme.}
\usage{
  theme_nothing(base_size = 12)
}
\arguments{
  \item{base_size}{base size, not used.}
}
\value{
  a ggplot2 theme (i.e., a list of class options).
}
\description{
  theme_nothing simply strips all thematic element in
  ggplot2 for map plotting.
}
\examples{
library(ggplot2)
n <- 50
df <- expand.grid(x = 1:n,y = 1:n)[sample(n^2,.5*n^2),]
qplot(x, y, data = df, geom = 'tile')
qplot(x, y, data = df, geom = 'tile') + theme_nothing()
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}

