#' Manage the ggmap file drawer.
#'
#' To avoid downloading the same maps again and again, ggmap uses a "file
#' drawer" to store the maps you've downloaded. By default, they will be
#' stored in a session specific temporary directory which will be deleted
#' each time you quit R. If you want them to persist across sessions,
#' set the global option "ggmap.file_drawer".
#'
#' @export
#' @keywords internal
#' @examples
#'
#' \dontrun{ if server is unresponsive, R check will fail
#'
#' file_drawer()
#' dir(file_drawer())
#'
#' # The first time you run this, the tiles are downloaded from the server
#' map <- get_stamenmap()
#' # The second time, the local cache is used so it's much faster
#' map <- get_stamenmap()
#'
#' dir(file_drawer())
#'
#' }
#'
file_drawer <- function(...) {
  fd <- getOption("ggmap.file_drawer", file.path(tempdir(), "ggmap"))
  file.path(fd, ...)
}

file_drawer_exists <- function() {
  file.exists(file_drawer())
}

file_drawer_create <- function() {
  if (file_drawer_exists()) return()

  dir.create(file_drawer(), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(), file_drawer("index.rds"))

  invisible(TRUE)
}

file_drawer_index <- function() {
  file_drawer_create()
  readRDS(file_drawer("index.rds"))
}

file_drawer_set <- function(url, map, name = NULL) {
  if (is.null(name)) {
    name <- paste0(digest::digest(url), '.rds')
  }

  index <- file_drawer_index()

  if (url %in% names(index)) {
    file.remove(index[[url]])
  }
  index[[url]] <- name
  saveRDS(index, file_drawer("index.rds"))
  saveRDS(map, file_drawer(name))

  invisible(TRUE)
}

file_drawer_get <- function(url) {
  index <- file_drawer_index()
  name <- index[[url]]

  if (is.null(name)) return(NULL)
  readRDS(file_drawer(name))
}
