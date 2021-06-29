context("ggmap")
source("util.R")

test_that("ggmap example works", {
  map <- getFakeMap()
  print(ggmap(map))
  expect_true(TRUE) # didn't stop: good!
})

test_that("ggmapplot example works", {
  map <- getFakeMap()
  expect_warning(
    # deprecated, and uses deprecated syntax
    # warns twice
    print(ggmapplot(map))
  )
  expect_true(TRUE) # didn't stop: good!
})
