context("test-hilo.R")

test_that("Extracting intervals from a distribution", {
  skip_if_not_installed("fable")
  
  hl <- hilo(fbl$.distribution, 80)
  
  expect_s3_class(hl, "hilo")
  expect_length(hl, 12)
})
