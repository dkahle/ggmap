\name{theme_nothing}
\alias{theme_nothing}
\title{Make a blank ggplot2 theme.}
\usage{
  theme_nothing(base_size = 12, legend = FALSE)
}
\arguments{
  \item{base_size}{base size, not used.}

  \item{legend}{should the legend be included?}
}
\value{
  a ggplot2 theme (i.e., a list of class options).
}
\description{
  theme_nothing simply strips all thematic element in
  ggplot2.
}
\examples{
library(ggplot2)
\dontrun{

n <- 50
df <- expand.grid(x = 1:n,y = 1:n)[sample(n^2,.5*n^2),]
p <- qplot(x, y, data = df, geom = 'tile')
p
p + theme_nothing()
p + theme_nothing(legend = TRUE) # no difference
p +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_nothing()

qplot(1:10,1:10) +
  theme_nothing()  +
  theme(panel.background = element_rect(fill = "black"))


df$class <- factor(sample(0:1, .5*n^2,  replace = TRUE))
p <- qplot(x, y, data = df, geom = "tile", fill = class)
p
p + theme_nothing()
p + theme_nothing(legend = TRUE)

p <- p +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))
p
p + theme_nothing()
p + theme_nothing(legend = TRUE)

}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}

