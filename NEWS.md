# ggmap 3.0.0.900

## Minor improvements and fixes

*   ___ggmap__ functions can now be called even if it has not been attached to 
    the search path (loaded) (reported by @jennybc, #264)

*   `mapdist()` now properly orders results (reported by @BirgerNi, #266).

*   `get_map()` now (again) respects the `source = "google"` specification when 
    given a bounding box (reported by @julovi, #267).

*   `geocode()` now (again) respects the `source = "dsk"` specification 
    (reported by @alistaire47, #180).

