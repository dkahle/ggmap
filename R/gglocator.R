#' Locator for ggplots.
#'
#' Locator for ggplots.
#' 
#' @param n number of points to locate.
#' @param object plot to locate on
#' @param message turn messaging from grid.ls on/off
#' @param xexpand expand argument in scale_x_continuous
#' @param yexpand expand argument in scale_y_continuous
#' @return a data frame with columns according to the x and y aesthetics
#' @author Tyler Rinker with help from Baptiste Auguie and StackOverflow user DWin with additions and canning by David Kahle \email{david.kahle@@gmail.com}.
#' @export
#' @examples
#' 
#' 
#' \dontrun{
#' df <- data.frame(xvar = 1:10, yvar = 1:10)
#' qplot(xvar, yvar, data = df) + annotate(geom = 'point', x = 3, y = 6)
#' gglocator(4)
#' 
#' qplot(xvar, yvar, data = df) + 
#'   scale_x_continuous(expand = c(0,0)) + 
#'   scale_y_continuous(expand = c(0,0))
#' gglocator(1, xexpand = c(0,0), yexpand = c(0,0))
#' 
#' }
#' 
#'   
gglocator <- function(n = 1, object = last_plot(), message = FALSE, xexpand = c(.05,0), yexpand = c(.05, 0)){
  
  if(n > 1){
    df <- NULL
    for(k in 1:n){
      df <- rbind(df, gglocator(object = object, message = message, 
        xexpand = xexpand, yexpand = yexpand))
    }
    return(df)
  }
  
  x <- grid.ls(print = message)[[1]]
  x <- x[ grep("panel-", grid.ls(print=message)[[1]]) ] #locate the panel
  seekViewport(x)
  loc <-  as.numeric(grid.locator("npc"))

  xrng <- with(object, range(data[,deparse(mapping$x)]))
  yrng <- with(object, range(data[,deparse(mapping$y)]))    
    
  xrng <- scales::expand_range(range = xrng, mul = xexpand[1], add = xexpand[2])
  yrng <- scales::expand_range(range = yrng, mul = yexpand[1], add = yexpand[2])    

  point <- data.frame(xrng[1] + loc[1]*diff(xrng), yrng[1] + loc[2]*diff(yrng))
  names(point) <- with(object, c(deparse(mapping$x), deparse(mapping$y)))
  point
}