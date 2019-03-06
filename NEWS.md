# ggmap 3.0.0.900

## Minor improvements and fixes

*   ___ggmap__ functions can now be called even if it has not been attached to 
    the search path (loaded) (reported by @jennybc, @lorenzwalthert, #264, 
    #244).
    
*   `get_stamenmap()` now only returns URLs if `messaging = TRUE` (Reported by
    @ikosmidis, #274). It also includes a more formal attribution to Stamen 
    Design and OpenStreetMap.
    
*   `get_stamenmap()` now supports a SSL Stamen endpoint via the `https` 
    argument (#276).

*   `mapdist()` now properly orders results (reported by @BirgerNi, #266).

*   `get_map()` now (again) respects the `source = "google"` specification when 
    given a bounding box (reported by @julovi, #267).

*   `geocode()` now (again) respects the `source = "dsk"` specification 
    (reported by @alistaire47, #180).

