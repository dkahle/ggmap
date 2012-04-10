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
\dontrun{

n <- 50
df <- expand.grid(x = 1:n,y = 1:n)[sample(n^2,.5*n^2),]
qplot(x, y, data = df, geom = 'tile')
qplot(x, y, data = df, geom = 'tile') + theme_nothing()
qplot(x, y, data = df, geom = 'tile') +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_nothing()

qplot(1:10,1:10) +
  theme_nothing()  +
  opts(panel.background = theme_rect(fill = 'black'))

}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}

