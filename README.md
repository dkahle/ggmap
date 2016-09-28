<!-- README.md is generated from README.Rmd. Please edit that file -->
ggmap
=====

**ggmap** makes it easy to retrieve raster map tiles from popular online mapping services like [Google Maps](https://developers.google.com/maps/documentation/static-maps/?hl=en), [OpenStreetMap](https://www.openstreetmap.org), [Stamen Maps](http://maps.stamen.com), and plot them using the **ggplot2** framework:

``` r
library(ggmap)

us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
ggmap(map)
```

![](figures/README-maptypes-1.png)

``` r
ggmap(map, extent = "device")
```

![](figures/README-maptypes-2.png)

Use `qmplot()` in the same way you'd use `qplot()`, but with a map automatically added in the background:

``` r
library(dplyr)

# only violent crimes
violent_crimes <- filter(crime, 
  offense != "auto theft", offense != "theft", offense != "burglary"
)

# rank violent crimes
violent_crimes$offense <- factor(
  violent_crimes$offense,
  levels = c("robbery", "aggravated assault", "rape", "murder")
)

# restrict to downtown
violent_crimes <- filter(violent_crimes,
  -95.39681 <= lon & lon <= -95.34188,
   29.73631 <= lat & lat <=  29.78400
)

qmplot(lon, lat, data = violent_crimes, maptype = "toner-lite", color = I("red"))
```

![](figures/README-qmplot-1.png)

``` r
qmplot(lon, lat, data = violent_crimes, maptype = "toner-lite", geom = "density2d", color = I("red"))
```

![](figures/README-qmplot-2.png)

Since **ggmap**'s built on top of **ggplot2**, all your usual **ggplot2** stuff (geoms, polishing, etc.) will work, and there are some unique graphing perks **ggmap** brings to the table, too.

``` r
robberies <- violent_crimes %>% filter(offense == "robbery")

qmplot(lon, lat, data = violent_crimes, geom = "blank", zoom = 15, maptype = "toner-background", darken = .7, legend = "topleft") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Robbery\nPropensity", low = "white", mid = "yellow", high = "red", midpoint = 650)
```

![](figures/README-styling-1.png)

Faceting works, too:

``` r
qmplot(lon, lat, data = violent_crimes, maptype = "toner-background", color = offense) + 
  facet_wrap(~ offense)
```

![](figures/README-faceting-1.png)

For convenience, here's a map of Europe:

``` r
europe <- c(left = -12, bottom = 35, right = 30, top = 63)
get_stamenmap(europe, zoom = 5) %>% ggmap()
```

![](figures/README-europe-1.png)

``` r
get_stamenmap(europe, zoom = 5, maptype = "toner-lite") %>% ggmap()
```

![](figures/README-europe-2.png)

Google Maps and Credentials
---------------------------

[Google Maps](http://developers.google.com/maps/terms) can be used just as easily. However, since Google Maps use a center/zoom specification, their input is a bit different:

``` r
get_googlemap("waco texas", zoom = 12) %>% ggmap()
#  Source : https://maps.googleapis.com/maps/api/staticmap?center=waco+texas&zoom=12&size=640x640&scale=2&maptype=terrain
#  Source : https://maps.googleapis.com/maps/api/geocode/json?address=waco%20texas
```

![](figures/README-google_maps-1.png)

Moreover, you can get various different styles of Google Maps with **ggmap** (just like Stamen Maps):

``` r
get_googlemap("waco texas", zoom = 12, maptype = "satellite") %>% ggmap()
#  Source : https://maps.googleapis.com/maps/api/staticmap?center=waco+texas&zoom=12&size=640x640&scale=2&maptype=satellite
#  Source : https://maps.googleapis.com/maps/api/geocode/json?address=waco%20texas
```

![](figures/README-google_styles-1.png)

``` r
get_googlemap("waco texas", zoom = 12, maptype = "roadmap") %>% ggmap()
#  Source : https://maps.googleapis.com/maps/api/staticmap?center=waco+texas&zoom=12&size=640x640&scale=2&maptype=roadmap
#  Source : https://maps.googleapis.com/maps/api/geocode/json?address=waco%20texas
```

![](figures/README-google_styles-2.png)

``` r
get_googlemap("waco texas", zoom = 12, maptype = "hybrid") %>% ggmap()
#  Source : https://maps.googleapis.com/maps/api/staticmap?center=waco+texas&zoom=12&size=640x640&scale=2&maptype=hybrid
#  Source : https://maps.googleapis.com/maps/api/geocode/json?address=waco%20texas
```

![](figures/README-google_styles-3.png)

Google's geocoding and reverse geocoding API's are available through `geocode()` and `revgeocode()`, respectively:

``` r
geocode("1301 S University Parks Dr, Waco, TX 76798")
#  Source : https://maps.googleapis.com/maps/api/geocode/json?address=1301%20S%20University%20Parks%20Dr%2C%20Waco%2C%20TX%2076798
#         lon      lat
#  1 -97.1161 31.55098
revgeocode(c(lon = -97.1161, lat = 31.55098))
#  Information from URL : https://maps.googleapis.com/maps/api/geocode/json?latlng=31.55098,-97.1161
#  [1] "1301 S University Parks Dr, Waco, TX 76706, USA"
```

Treks use Google's routing API to give you routes (`route()` and `trek()` give slightly different results; the latter hugs roads):

``` r
trek_df <- trek("houson, texas", "waco, texas", structure = "route")
#  Source : https://maps.googleapis.com/maps/api/directions/json?origin=houson%2C%20texas&destination=waco%2C%20texas&mode=driving&units=metric&alternatives=false
qmap("college station, texas", zoom = 8) +
  geom_path(
    aes(x = lon, y = lat),  colour = "blue",
    size = 1.5, alpha = .5,
    data = trek_df, lineend = "round"
  )
#  Source : https://maps.googleapis.com/maps/api/staticmap?center=college+station,+texas&zoom=8&size=640x640&scale=2&maptype=terrain&language=en-EN
#  Source : https://maps.googleapis.com/maps/api/geocode/json?address=college%20station%2C%20texas
```

![](figures/README-route_trek-1.png) (They also provide information on how long it takes to get from point A to point B.)

Map distances, in both length and anticipated time, can be computed with `mapdist()`). Moreover the function is vectorized:

``` r
mapdist(c("houston, texas", "dallas"), "waco, texas")
#  Source : https://maps.googleapis.com/maps/api/distancematrix/json?origins=dallas&destinations=waco%2C%20texas&mode=driving&language=en-EN
#  Source : https://maps.googleapis.com/maps/api/distancematrix/json?origins=houston%2C%20texas&destinations=waco%2C%20texas&mode=driving&language=en-EN
#              from          to      m      km     miles seconds   minutes
#  1 houston, texas waco, texas 298242 298.242 185.32758   10176 169.60000
#  2         dallas waco, texas 152652 152.652  94.85795    5291  88.18333
#       hours
#  1 2.826667
#  2 1.469722
```

### Google credentialing

If you have a Google API key, you can exceed the standard limits Google places on queries. By default, when **ggmap** is loaded it will set the following credentials and limits:

``` r
ggmap_credentials()
#  NULL
```

Look at the documentation of `?register_google()` to learn more. If you do have an API key, you set it with:

``` r
register_google(key = "[your key here]", account_type = "premium", day_limit = 100000)
ggmap_credentials()
#  Google - 
#     key : [your key here] 
#     account_type : premium 
#     day_limit : 1e+05
```

These will then be used and checked when creating the query URL:

``` r
register_google(key = "AbCdEfGhIjKlMnOpQrStUvWxYz")
# get_googlemap("waco texas", urlonly = TRUE)
```

For anything that hasn't been implemente (URL-wise), you can inject code into the query usin g `inject`:

``` r
# get_googlemap("waco texas", urlonly = TRUE, inject = "otherItem = Stuff")
```

Installation
------------

-   From CRAN: `install.packages("ggmap")`

-   From Github: `devtools::install_github("dkahle/ggmap")`
