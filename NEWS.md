# ggmap 3.0.1

## New features

*   `geocode_cache()` is now exported to the user, and functions 
    `write_geocode_cache()` and `write_geocode_cache()` exist to facilitate 
    using the same cache across sessions.
    
## Changes

*   __ggmap__ no longer depends on __rjson__ (thanks @MichaelChirico, #317). 
*   Most functions now use **cli**-based messaging functions, following 
    **ggplot2**'s lead.

## Minor improvements and fixes

*   __ggmap__ functions can now be called even if it has not been attached to 
    the search path (loaded) (reported by @jennybc, @lorenzwalthert, #264, 
    #244).
    
*   Google functions now properly encode #'s (reported by @aaronrudkin, #272).
    
*   `get_stamenmap()` now only returns URLs if `messaging = TRUE` (Reported by
    @ikosmidis, #274). It also includes a more formal attribution to Stamen 
    Design and OpenStreetMap.
    
*   `get_stamenmap()` now supports a SSL Stamen endpoint via the `https` 
    argument (#276).

*   `mapdist()` now properly orders results (reported by @BirgerNi, #266).

*   `mapdist()` now properly returns URLs if `urlonly = TRUE`.

*   `get_map()` now (again) respects the `source = "google"` specification when 
    given a bounding box (reported by @julovi, #267).

*   `geocode()` now (again) respects the `source = "dsk"` specification 
    (reported by @alistaire47, #180).
    
*   A bug giving an error message `Error in aperm.default(map, c(2, 1, 3)) : invalid first argument, must be an array` has now been fixed. (solution by @kent37, #262).

*   `geocode()` now properly caches when geocoding several locations. The 
    caching keys are now the hash values of the scrubbed url.

