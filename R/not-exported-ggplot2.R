# is.constant <- get("is.constant", envir = asNamespace("ggplot2"))
is.constant <- function (x) {
  is_I_call <- function(x) is.call(x) && identical(x[[1]], quote(I))
  vapply(x, is_I_call, logical(1))
}






# rename_aes <- get("rename_aes",  envir = asNamespace("ggplot2"))
rename_aes <- function (x) {
  names(x) <- standardise_aes_names(names(x))
  duplicated_names <- names(x)[duplicated(names(x))]
  if (length(duplicated_names) > 0L) {
    cli::cli_warn("Duplicated aesthetics after name standardisation: {.field {unique0(duplicated_names)}}")
  }
  x
}

# rename_aes() requires standardise_aes_names()
standardise_aes_names <- function(x) {
  x <- sub("color", "colour", x, fixed = TRUE)
  revalue(x, ggplot_global__base_to_ggplot)
}

# standardise_aes_names() requires revalue()
revalue <- function (x, replace) {
  if (is.character(x)) {
    replace <- replace[names(replace) %in% x]
    if (length(replace) == 0)
      return(x)
    x[match(names(replace), x)] <- replace
  }
  else if (is.factor(x)) {
    lev <- levels(x)
    replace <- replace[names(replace) %in% lev]
    if (length(replace) == 0) return(x)
    lev[match(names(replace), lev)] <- replace
    levels(x) <- lev
  }
  else if (!is.null(x)) {
    cli::cli_abort("{.arg x} must be a factor or character vector")
  }
  x
}

# standardise_aes_names() requires ggplot_global$base_to_ggplot
ggplot_global__base_to_ggplot <- c(
  col = "colour",
  color = "colour",
  pch = "shape",
  cex = "size",
  lty = "linetype",
  lwd = "size",
  srt = "angle",
  adj = "hjust",
  bg = "fill",
  fg = "colour",
  min = "ymin",
  max = "ymax"
)



# .all_aesthetics <- get(".all_aesthetics", envir = asNamespace("ggplot2"))
.all_aesthetics <- c("adj", "alpha", "angle", "bg", "cex", "col", "color", "colour",
  "fg", "fill", "group", "hjust", "label", "linetype", "lower",
  "lty", "lwd", "max", "middle", "min", "pch", "radius", "sample",
  "shape", "size", "srt", "upper", "vjust", "weight", "width",
  "x", "xend", "xmax", "xmin", "xintercept", "y", "yend", "ymax",
  "ymin", "yintercept", "z")


