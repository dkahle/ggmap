context("ggmap")
source("util.R")

test_that("ggmap example works", {
  map <- getFakeMap()
  ggmap(map)
  expect_true(TRUE) # didn't stop: good!
})

test_that("ggmapplot example works", {
  map <- getFakeMap()
  expect_warning(
    # deprecated, and uses deprecated syntax
    # warns twice
    ggmapplot(map)
  )
  expect_true(TRUE) # didn't stop: good!
})
