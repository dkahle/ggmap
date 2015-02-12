# With help from
# https://github.com/leaflet-extras/leaflet-providers/blob/master/leaflet-providers.js

osm_license <- paste0('<a href="http://openstreetmap.org">OpenStreetMap</a>,',
  '<a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>')

provider <- function(tile_f, attr_tile, attr_data = osm_license) {
  structure(
    list(
      tile_f = tile_f,
      attr_tile = attr_tile,
      attr_data = attr_data
    ),
    class = "slippymap_provider"
  )
}

providers <- list(
  stamen = provider(
    function(x, y, z, style = "terrain") {
      sprintf("http://tile.stamen.com/%s/%i/%i/%i.png", style, z, x, y)
    },
    paste0('<a href="http://stamen.com">Stamen Design</a>',
      '(<a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>')
  ),
  open_street_map = provider(
    function(x, y, z) {
      sprintf('http://a.tile.openstreetmap.org/%i/%i/%i.png', x, y, z)
    },
    osm_license
  ),
  cloudmade = provider(
    function(x, y, z, style, key) {
      sprintf("http://tile.cloudmade.com/%s/%s/256/%i/%i/%i.png", key, style, z, x, y)
    },
    ""
  ),
  cartodb = provider(
    function(x, y, z, style = "terrain") {
      sprintf("http://basemaps.cartocdn.com/%s/%i/%i/%i.png", style, z, x, y)
    },
    "&copy <a href='CartoDb'>http://cartodb.com/attributions</a>"

  )
)
