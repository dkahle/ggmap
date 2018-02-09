test_that("crime data classes look right", {
  data(crime)
  expect_equal(class(crime$time), c("POSIXct", "POSIXt"))
})
