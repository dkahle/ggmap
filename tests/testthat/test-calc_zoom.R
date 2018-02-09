context("calc_zoom")

test_that("calc_zoom example works", {
  zoom <- calc_zoom(
    c(-94, -95),
    c(56, 57)
  )
  expect_equal(zoom, 10)
})