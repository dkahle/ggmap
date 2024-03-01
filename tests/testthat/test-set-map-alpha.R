context("alpha")
source("util.R")

test_that("adds alpha layer", {
  map <- getFakeMap()
  map <- set_map_alpha(map, 0.5)
  expect_identical(as.character(map), rep("#FFFFFF80", 4))
})
