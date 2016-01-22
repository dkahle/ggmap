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

![](README-maptypes-1.png)

``` r
ggmap(map, extent = "device")
```

![](README-maptypes-2.png)

Use `qmplot()` in the same way you'd use `qplot()`, but with a map automatically added in the background:

``` r
downtown <- subset(crime,
  -95.39681 <= lon & lon <= -95.34188 &
   29.73631 <= lat & lat <=  29.78400
)

qmplot(lon, lat, data = downtown, maptype = "toner-background", color = I("red"))
```

![](README-qmplot-1.png)

``` r
qmplot(lon, lat, data = downtown, maptype = "toner-lite", geom = "density2d", color = I("red"))
```

![](README-qmplot-2.png)

Since **ggmap**'s built on top of **ggplot2**, all your usual **ggplot2** stuff (geoms, polishing, etc.) will work, and there are some unique graphing perks **ggmap** brings to the table, too.

``` r
robberies <- subset(downtown, offense == "robbery")

qmplot(lon, lat, data = downtown, geom = "blank", zoom = 15, maptype = "toner-background", darken = .7) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = .3, color = NA) +
  scale_fill_gradient2("Robbery\nPropensity", low = "white", mid = "yellow", high = "red", midpoint = 1500)
```

![](README-styling-1.png)

Faceting works, too:

``` r
qmplot(lon, lat, data = downtown, maptype = "toner-background", color = offense) + 
  facet_wrap(~ offense)
```

![](README-faceting-1.png)

For convenience, here's a map of Europe:

``` r
europe <- c(left = -12, bottom = 35, right = 30, top = 63)
map <- get_stamenmap(europe, zoom = 5, maptype = "toner-lite")
ggmap(map)
```

![](README-europe-1.png)

Installation
------------

-   From CRAN: `install.packages("ggmap")`

-   From Github: `devtools::install_github("dkahle/ggmap")`
