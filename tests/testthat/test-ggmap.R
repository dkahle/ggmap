context("ggmap")
source("util.R")

test_that("ggmap example works", {
  skip("FIXME: Doesn't work with cran ggplot. When fixed, remove skip.")
  map <- getFakeMap()
  ggmap(map)
})

test_that("ggmapplot example works", {
  skip("FIXME: Doesn't work with cran ggplot. When fixed, remove skip.")
  map <- getFakeMap()
  ggmapplot(map)
})
