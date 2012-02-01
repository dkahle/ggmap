\name{ggimage}
\alias{ggimage}
\title{Plot an image using ggplot2}
\usage{
  ggimage(mat, fullpage = TRUE, verbose = FALSE,
    coord_equal = TRUE)
}
\arguments{
  \item{mat}{an image object (from e.g. read.jpeg in the
  ReadImages package)}

  \item{fullpage}{logical; should the image take up the
  entire viewport?}

  \item{verbose}{logical; message the user?}

  \item{coord_equal}{logical; should the axes units be
  equal?}
}
\value{
  a ggplot object
}
\description{
  ggimage is the near ggplot2 equivalent of image.
}
\examples{
\dontrun{
img <- matrix(1:16, 4, 4)
image(img)
ggimage(t(img[,4:1]), fullpage = FALSE) +
  scale_fill_gradientn(colours = heat.colors(10), guide = 'none')


data(hadley)
ggimage(hadley, verbose = TRUE)

x <- seq(1, 438, 15); n <- length(x)
df <- data.frame(x = x, y = 120*(scale((x - 219)^3 - 25000*x) + rnorm(n)/2 -3))
qplot(x, y, data = df, geom = c('smooth','point'))
ggimage(hadley, fullpage = FALSE) +
  geom_smooth(aes(x = x, y = y), fill = I('gray60'), data = df,
    colour = I('green'), size = I(1)) +
  geom_point(aes(x = x, y = y), data = df,
    colour = I('green'), size = I(3), fill = NA)
}
}
\author{
  David Kahle \email{david.kahle@gmail.com}
}

