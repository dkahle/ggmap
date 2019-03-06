context("URLs are properly encoded")

test_that("# encodes correctly", {

  Sys.setenv("GGMAP_GOOGLE_API_KEY" = "mQkzTpiaLYjP_XQBotesgif3EfGL2dbrNVOrogg") # fake key

  expect_equal(
    get_googlemap("#10 downing street, london", urlonly = TRUE),
    "https://maps.googleapis.com/maps/api/staticmap?center=%2310%20downing%20street,%20london&zoom=10&size=640x640&scale=2&maptype=terrain&key=xxx"
  )

  expect_equal(
    geocode("10 downing street, london", urlonly = TRUE),
    "https://maps.googleapis.com/maps/api/geocode/json?address=10+downing+street,+london&key=xxx"
  )

  expect_equal(
    route("#10 downing street, london", "buckingham palace", urlonly = TRUE),
    "https://maps.googleapis.com/maps/api/directions/json?origin=%2310+downing+street,+london&destination=buckingham+palace&key=xxx&mode=driving&alternatives=false&units=metric"
  )

  expect_equal(
    trek("#10 downing street, london", "buckingham palace", urlonly = TRUE),
    "https://maps.googleapis.com/maps/api/directions/json?origin=%2310+downing+street,+london&destination=buckingham+palace&key=xxx&mode=driving&alternatives=false&units=metric"
  )

  expect_equal(
    mapdist("#10 downing street, london", "buckingham palace", urlonly = TRUE),
    "https://maps.googleapis.com/maps/api/distancematrix/json?origins=%2310+downing+street,+london&destinations=buckingham+palace&key=xxx&mode=driving"
  )


})
