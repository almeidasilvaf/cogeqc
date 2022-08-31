

#----Start tests----------------------------------------------------------------
test_that("is_valid() returns a logical scalar", {
    v <- is_valid("echo")
    expect_equal(class(v), "logical")
})

test_that("busco_is_installed() returns logical values", {
  l <- busco_is_installed()
  expect_equal(class(l), "logical")
})
