
test_that("busco_is_installed() returns logical values", {
  l <- busco_is_installed()
  expect_equal(class(l), "logical")
})
