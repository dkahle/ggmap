#' Quick map plot
#'
#' qmap is a wrapper for \code{\link{ggmapplot}} and \code{\link{ggmap}}.
#' 
#' @param ... stuff to pass to \code{\link{ggmapplot}} and \code{\link{ggmap}}.
#' @return a ggplot object
#' @author David Kahle \email{david.kahle@@gmail.com}
#' @seealso \code{\link{ggmapplot}} and \code{\link{ggmap}}.
#' @export
#' @examples
#'
#' 
#' \dontrun{
#' qmap(location = 'waco')
#' qmap(location = 'waco', scale = 17)
#' qmap(location = 'waco', scale = 17, fullpage = TRUE, verbose = TRUE)
#'
#' qmap(location = 'paris', source = 'osm', scale = 600000)
#' qmap(location = 'paris', source = 'osm', scale = 400000)
#' qmap(location = 'paris', source = 'osm', scale = 200000)
#' }
#' 
qmap <- function(...){
  args <- as.list(match.call(expand.dots = TRUE)[-1])	
  
  if('fullpage' %in% names(args)){
    fullpage <- args$fullpage
  } else {
  	fullpage <- TRUE
  }
    
  ggmapplot(ggmap(...), fullpage = fullpage)
}
