context("geocode")

register_google(key = Sys.getenv("GOOGLE_GEOCODE_KEY"))

test_that("geocode(urlonly = TRUE) returns url.", {
  res <- geocode("Houston, Texas", urlonly = TRUE)
  expect_is(res, "character")
  expect_match(res, "Houston%2C%20Texas", fixed = TRUE)
  expect_message(res, regexp = NA) # No "Source : ..." message
})

test_that("geocode(urlonly = TRUE) works with multiple locations.", {
  res <- geocode(c("Houston, Texas", "Washington, DC"), urlonly = TRUE)
  expect_match(res[[1, 1]], "Houston%2C%20Texas", fixed = TRUE)
  expect_match(res[[2, 1]], "Washington%2C%20DC", fixed = TRUE)
  expect_message(res, regexp = NA) # No "Source : ..." message, see #207
})

test_that("geocode(ext) works with multiple locations.", {
  res <- geocode(c("Houston", "Washington"), urlonly = TRUE, ext = "ca")
  expect_match(res[[1]], ".ca", fixed = TRUE)
  expect_match(res[[2, 1]], ".ca", fixed = TRUE)
  expect_message(res, regexp = NA) # No "Source : ..." message, see #207
})
