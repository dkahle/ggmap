#' @import ggplot2
#' @docType package
#' @name ggmap
#' @importFrom RgoogleMaps XY2LatLon
#' @importFrom png readPNG
#' @importFrom jpeg readJPEG
#' @importFrom plyr arrange compact ddply desc dlply is.discrete is.formula join
#'   ldply llply .
#' @importFrom bitops bitOr bitShiftL bitShiftR bitAnd
#' @importFrom grDevices as.raster extendrange gray rgb
#' @importFrom stats time asOneSidedFormula
#' @importFrom utils URLencode download.file tail
#' @importFrom grid rasterGrob seekViewport grid.locator upViewport downViewport
#'   current.vpTree current.vpPath viewport
#' @importFrom scales expand_range
#' @importFrom dplyr bind_cols filter bind_rows mutate group_by ungroup select
#'   right_join
#' @importFrom glue glue
#' @importFrom httr GET http_status warn_for_status stop_for_status
#' @importFrom tibble tibble as_tibble
#' @importFrom stringr str_to_title str_c str_detect str_c str_replace_all
#'   str_sub str_trim str_pad str_split str_trunc str_detect str_replace
#'   str_extract
#' @importFrom purrr map map_chr map_int walk flatten pluck imap cross_df
#'   flatten_chr
#' @importFrom rlang set_names
#' @importFrom magrittr %>%
#' @importFrom tidyr nest unnest spread
#' @importFrom cli cli_warn cli_abort cli_alert_info cli_alert_danger
#' @aliases ggmap package-ggmap
NULL
