#' Plot an image using ggplot2
#'
#' ggimage is the near ggplot2 equivalent of image.
#' 
#' @param mat an image object (from e.g. read.jpeg in the ReadImages package)
#' @param fullpage logical; should the image take up the entire viewport?
#' @param verbose logical; message the user?
#' @param coord_equal logical; should the axes units be equal?
#' @return a ggplot object
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @export
#' @examples
#' img <- matrix(1:16, 4, 4)
#' image(img)
#' ggimage(t(img[,4:1]), fullpage = FALSE) + 
#'   scale_fill_gradientn(colour = heat.colors(10), legend = FALSE)
#' 
#' \dontrun{
#' data(hadley)
#' ggimage(hadley, verbose = TRUE) 
#' 
#' x <- seq(1, 438, 15); n <- length(x)
#' df <- data.frame(x = x, y = 120*(scale((x - 219)^3 - 25000*x) + rnorm(n)/2 -3))
#' qplot(x, y, data = df, geom = c('smooth','point'))
#' ggimage(hadley, fullpage = FALSE) + 
#'   geom_smooth(aes(x = x, y = y), fill = I('gray60'), data = df,
#'     colour = I('green'), size = I(1)) +
#'   geom_point(aes(x = x, y = y), data = df, 
#'     colour = I('green'), size = I(3), fill = NA)
#' }
#'
ggimage <- function(mat, fullpage = TRUE, verbose = FALSE, coord_equal = TRUE){
  require(ggplot2)
  require(reshape2)
  
  # dummies to trick R CMD check   
  column <- NULL; rm(column); row <- NULL; rm(row); fill <- NULL; rm(fill); 
  
  if(!any(class(mat) %in% c('matrix','imagematrix','array'))){
  	stop('mat should be a matrix or an array.')
  }
	
  if(length(dim(mat)) == 2){
    if(verbose) message('creating black and white image...', appendLF = FALSE)
    mat <- reshape2::melt(mat)
  	if(verbose) message('done.')    
    names(mat) <- c('row','column','fill')
    plot <- qplot(column, -row, data = mat, geom = 'tile', fill = fill) +
      scale_fill_gradient(low = 'black', high = 'white', legend = FALSE)
  }
  
  if(length(dim(mat)) == 3){
  	if(verbose) message('creating color image... ', appendLF = FALSE)
  	mat <- apply(mat, 1:2, function(v) .Internal(rgb(v[1], v[2], v[3], 1, 1, NULL)))    
  	if(verbose) message('done.')
    mat <- reshape2::melt(mat)
    names(mat) <- c('row', 'column', 'fill')
    plot <- qplot(column, -row, data = mat, geom = 'tile', fill = fill) +
      scale_fill_identity(legend = FALSE)  	
  }

  if(fullpage) plot <- plot + theme_nothing()
  
  if(coord_equal) plot <- plot + coord_equal()
  
  plot
}