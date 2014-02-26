#' Check for a ggmap file cabinet
#'
#' Check for a ggmap file cabinet
#'
#' Changing the arguments is not recommended. 
#' 
#' @param where where the directory should be made
#' @param drawerName the name of the directory
#' @return TRUE if found; FALSE if not found
#' @author David Kahle \email{david.kahle@@gmail.com}.
#' @export
#' @examples
#' 
#' \dontrun{
#' 
#' list.files()
#' file_drawer_found() # is the file drawer present?
#' make_file_drawer()
#' file_drawer_found()
#' list.files()
#' 
#' }
#' 
#'   
file_drawer_found <- function(where = getwd(), drawerName = 'ggmapFileDrawer'){

  drawerName %in% list.files(where)
  
}




#' Create a file cabinet for ggmap images
#'
#' Create a file cabinet for ggmap images
#'
#' Changing the arguments is not recommended. 
#' 
#' @param where where the directory should be made
#' @param drawerName the name of the directory
#' @return TRUE if directory created; FALSE if not
#' @author David Kahle \email{david.kahle@@gmail.com}.
#' @export
#' @examples
#' 
#' \dontrun{
#' 
#' list.files()
#' file_drawer_found() # is the file drawer present?
#' make_file_drawer()
#' file_drawer_found()
#' list.files()
#' list.files('ggmapFileDrawer')
#' get_database()
#'
#' }
#' 
#'   
make_file_drawer <- function(where = getwd(), drawerName = 'ggmapFileDrawer'){

  startwd <- getwd()

  # check for name return NULL if present
  if(file_drawer_found(where, drawerName)) return(invisible(FALSE))
  
  # create drawer
  dir.create(paste(where, drawerName, sep = '/'))
  
  # make check-in sheet
  df <- data.frame(url = character(), file = character(), stringsAsFactors = FALSE)
  
  # move to where directory
  setwd(paste(where, drawerName, sep = '/'))
  
  # create database
  write.csv(df, file = 'ggmapsDatabase.csv', row.names = FALSE)
  
  # create ggmaps directory
  dir.create('ggmaps')
  
  # move back
  setwd(startwd)
  
  # return TRUE
  invisible(TRUE)
}









#' Get the ggmap database
#'
#' Get the ggmap database
#'
#' The ggmap database is a data frame with two columns : the url which needs to be called for the map (or maptile) and the image filename.
#' 
#' @param where where the directory should be made
#' @return a data frame
#' @author David Kahle \email{david.kahle@@gmail.com}.
#' @export
#' @examples
#' 
#' \dontrun{
#' 
#' list.files()
#' file_drawer_found() # is the file drawer present?
#' make_file_drawer()
#' file_drawer_found()
#' list.files()
#' str(get_database())
#' 
#' }
#' 
#'   
get_database <- function(where = paste(getwd(), 'ggmapFileDrawer', sep = '/')){

  if(!file_drawer_found()) stop('no filedrawer found.', call. = FALSE)

  # migrate to where
  startwd <- getwd()
  setwd(where)
  
  # read in database
  df <- read.csv('ggmapsDatabase.csv', header = TRUE, stringsAsFactors = FALSE)
  
  # migrate back to startwd
  setwd(startwd)
  
  # return
  df 
  
}







#' Add a new entry to the database
#'
#' Add a new entry to the database
#'
#' The ggmap database is a data frame with two columns : the url which needs to be called for the map (or maptile) and the image filename.
#' 
#' @param url url 
#' @param file file name
#' @param where where the directory should be made
#' @return a data frame
#' @author David Kahle \email{david.kahle@@gmail.com}.
#' @export
#' @examples
#' 
#' \dontrun{
#' 
#' get_database()
#' url <- 'http://maps.googleapis.com/maps/api/staticmap?'
#' url <- paste0(url, 'center=houston&zoom=10&size=%20640x640')
#' url <- paste0(url, '&scale=%202&maptype=terrain&sensor=false')
#' update_database(url) # automatically created file drawer
#' get_database()
#' 
#' # add it again with a new file name
#' update_database(url)
#' get_database()
#'
#' # add a different url
#' update_database("new url")
#' get_database()
#' 
#' }
#' 
#'   
update_database <- function(url, file = paste0(timeStamp(), '.rds'), where = paste(getwd(), 'ggmapFileDrawer', sep = '/')){

  if(!file_drawer_found()) make_file_drawer()

  # migrate to where
  startwd <- getwd()
  setwd(where)
  
  # read in database
  df <- read.csv('ggmapsDatabase.csv', header = TRUE, stringsAsFactors = FALSE)
  
  # check if url is in database.  if it is, update the filename.  if not, add.
  if(url %in% df$url){
  	oldfile <- df$file[which(df$url == url)]
  	file.remove(paste("ggmaps", oldfile, sep = '/'))
    df$file[which(df$url == url)] <- file
    
  } else {
    df <- rbind(df, data.frame(url = url, file = file, stringsAsFactors = FALSE))
  }
  
  # create database
  write.csv(df, file = 'ggmapsDatabase.csv', row.names = FALSE)  
  
  # migrate back to startwd
  setwd(startwd)
  
  invisible(NULL)  
}






#' Lookup the file name of a map from a specified url
#'
#' Lookup the file name of a map from a specified url
#'
#' @param url name of the map desired
#' @param where where argument to pass to get_database
#' @return filename (e.g. filename.rds if the map is on file) or FALSE (if the map is not on file)
#' @author David Kahle \email{david.kahle@@gmail.com}.
#' @export
#' @examples
#' 
#' \dontrun{
#' 
#' get_database()
#' url <- 'http://maps.googleapis.com/maps/api/staticmap?'
#' url <- paste0(url, 'center=houston&zoom=10&size=%20640x640')
#' url <- paste0(url, '&scale=%202&maptype=terrain&sensor=false')
#' file <- paste0(timeStamp(), '.rds')
#' update_database(url, file)
#' get_database()
#' url_lookup(url)
#'
#' url_lookup('an unknown url')
#' 
#' }
#' 
#'   
url_lookup <- function(url, where = paste(getwd(), 'ggmapFileDrawer', sep = '/')){

  if(!file_drawer_found(where)) make_file_drawer()

  # read in database
  df <- get_database(where)

  # find index of url 
  urlNdx <- which(df$url == url)
  
  # return false if not found
  if(length(urlNdx) == 0) return(FALSE)
  
  # return filename.
  df$file[urlNdx]
  
}

















#' Store a ggmap object
#'
#' Store a ggmap object
#' 
#' @param ggmap a ggmap object
#' @param url url that map came from
#' @param file file name (with extension .rds)
#' @param where the ggmap file drawer
#' @return a data frame
#' @author David Kahle \email{david.kahle@@gmail.com}.
#' @export
#' @examples
#' 
#' \dontrun{
#' 
#' 
#' list.files(recursive = TRUE)
#' map <- get_map()
#' url <- 'http://maps.googleapis.com/maps/api/staticmap?'
#' url <- paste0(url, 'center=houston&zoom=10&size=%20640x640')
#' url <- paste0(url, '&scale=%202&maptype=terrain&sensor=false')
#' str(map)
#' 
#' list.files(recursive = TRUE)
#' get_database() # no database
#' archive_ggmap(map, url)
#' get_database()
#' list.files(recursive = TRUE)
#' 
#' 
#' }
#' 
#'   
archive_ggmap <- function(ggmap, url, file = paste0(timeStamp(), '.rds'), 
  where = paste(getwd(), 'ggmapFileDrawer', sep = '/'))
{

  if(!file_drawer_found()) make_file_drawer()
  
  # update database
  update_database(url, file)

  # migrate to where
  startwd <- getwd()
  setwd(where)
  
  # save
  saveRDS(ggmap, paste("ggmaps", file, sep = '/'))
  
  # migrate back to startwd
  setwd(startwd)
  
  invisible(NULL)  
}















#' Retrieve an archived ggmap object by url
#'
#' Retrieve an archived ggmap object by url
#' 
#' @param url a url with a map
#' @param where the ggmap file drawer
#' @return a data frame
#' @author David Kahle \email{david.kahle@@gmail.com}.
#' @export
#' @examples
#' 
#' \dontrun{
#' 
#' list.files(recursive = TRUE)
#' map <- get_map()
#' url <- 'http://maps.googleapis.com/maps/api/staticmap?'
#' url <- paste0(url, 'center=houston&zoom=10&size=%20640x640')
#' url <- paste0(url, '&scale=%202&maptype=terrain&sensor=false')
#' ggmap(map)
#' 
#' archive_ggmap(map, url)
#' list.files(recursive = TRUE)
#'
#' rm(map)
#' ggmap(map) # error
#' map <- recall_ggmap(url)
#' ggmap(map)
#' 
#' }
#' 
#'   
recall_ggmap <- function(url, where = paste(getwd(), 'ggmapFileDrawer', sep = '/')){

  if(!file_drawer_found()) stop('no filedrawer found.', call. = FALSE)
  
  # determine the filename to get
  file <- url_lookup(url)
  
  # migrate to where
  startwd <- getwd()
  setwd(where)  
  
  # save
  out <- readRDS(paste("ggmaps", file, sep = '/'))
  
  # migrate back to startwd
  setwd(startwd)
  
  out
}

































#' Convenient time grabber
#'
#' Convenient time grabber
#' 
#' @return time as a character string
#' @author David Kahle \email{david.kahle@@gmail.com}.
#' @export
#' @examples
#' 
#' \dontrun{
#' 
#' timeStamp()
#' str(timeStamp())
#' 
#' }
#' 
#'   
timeStamp <- function(){
  s <- Sys.time()
  s <- gsub(' ', '-', s)
  s <- gsub(':', '-', s)
  s  
}