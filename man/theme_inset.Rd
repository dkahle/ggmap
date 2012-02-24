\name{theme_inset}
\alias{theme_inset}
\title{Make a ggplot2 inset theme.}
\usage{
  theme_inset(base_size = 12)
}
\arguments{
  \item{base_size}{base size, not used.}
}
\value{
  a ggplot2 theme (i.e., a list of class options).
}
\description{
  theme_inset is a ggplot2 theme geared towards making
  inset plots.
}
\examples{
library(ggplot2)
n <- 50
df <- expand.grid(x = 1:n,y = 1:n)[sample(n^2,.5*n^2),]
qplot(x, y, data = df, geom = 'tile')
qplot(x, y, data = df, geom = 'tile') + theme_nothing()

qplot(1:10, 1:10) +
  annotation_custom(
    grob = ggplotGrob(qplot(1:10,1:10)),
    8, Inf, -Inf, 2
  )

qplot(1:10, 1:10) +
  annotation_custom(
    grob = ggplotGrob(qplot(1:10,1:10) + theme_nothing()),
    8, Inf, -Inf, 2
  )

qplot(1:10, 1:10) +
  annotation_custom(
    grob = ggplotGrob(qplot(1:10,1:10) + theme_inset()),
    8, Inf, -Inf, 2
  )
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}

