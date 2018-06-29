context("LonLat2XY")

test_that("LonLat2XY example works", {
  # gc <- geocode('baylor university')
  gc <- list(lon = -97.11431, lat = 31.54984)
  ll2xy <- LonLat2XY(gc$lon, gc$lat, 10)
  expect_equal(
    ll2xy$X,
    235
  )
  expect_equal(
    ll2xy$Y,
    417
  )
  expect_true(
    abs(ll2xy$x - 195.5142) < 0.01 # float math is hard
  )
  expect_true(
    abs(ll2xy$y - 88.52267) < 0.01 # float math is hard
  )
})