#' Plot an image using ggplot2
#'
#' ggimage is the near ggplot2 equivalent of image.
#'
#' @param mat a matrix, imagematrix, array, or raster (something
#'   that can be coerced by as.raster)
#' @param fullpage should the image take up the entire viewport?
#' @param coord_equal should the axes units be equal?
#' @param scale_axes should the axes be
#'   [0,ncol(mat)-1]x[0,nrow(mat)-1] (F) or [0,1]x[0,1] (T)
#' @return a ggplot object
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @export
#' @examples
#'
#' img <- matrix(1:16, 4, 4)
#' image(img)
#' ggimage(t(img[,4:1]), fullpage = FALSE, scale_axes = TRUE)
#' ggimage(t(img[,4:1]), fullpage = FALSE)
#'
#'
#' \dontrun{
#' # not run due to slow performance
#'
#' data(hadley)
#' ggimage(hadley)
#' ggimage(hadley, coord_equal = FALSE)
#'
#' x <- seq(1, 438, 15); n <- length(x)
#' df <- data.frame(x = x, y = -(120*(scale((x - 219)^3 - 25000*x) + rnorm(n)/2 - 3)))
#' qplot(x, y, data = df, geom = c('smooth','point'))
#' ggimage(hadley, fullpage = FALSE) +
#'   geom_smooth(aes(x = x, y = y), fill = I('gray60'), data = df,
#'     colour = I('green'), size = I(1)) +
#'   geom_point(aes(x = x, y = y), data = df,
#'     colour = I('green'), size = I(3), fill = NA)
#'
#' }
ggimage <- function(mat, fullpage = TRUE, coord_equal = TRUE, scale_axes = FALSE){

  x <- NULL; rm(x); y <- NULL; rm(y);

  if(!any(class(mat) %in% c('matrix','imagematrix','array','raster'))){
  	stop('mat should be a matrix or an array.')
  }

  n <- nrow(mat)
  p <- ncol(mat)

  if(scale_axes) n <- p <- 2

  fourCorners <- expand.grid(x = 0:(p-1), y = 0:(n-1))

  if(max(mat) > 1 || min(mat) < 0){
    message('rescaling mat to [0,1]...')
    mat <- (mat - min(mat)) / (max(mat) - min(mat))
  }

  raster <- as.raster(mat)

  plot <- ggplot(aes(x, y), data = fourCorners) + geom_blank() +
    #ggmap:::annotation_raster(raster, 0, p-1, 0, n-1) +
    annotation_raster(raster, 0, p-1, 0, n-1) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))

  if(fullpage) plot <- plot + theme_nothing()

  if(coord_equal) plot <- plot + coord_equal()

  plot
}
