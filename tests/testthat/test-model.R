test_that("model validates declaration", {
  expect_error(cssem_model(list(A = list(indicators = "x", scales = "ordinal"))))
  expect_error(cssem_model(list(A = list(indicators = c("x", "y"), scales = "bad"))))
  expect_s3_class(cssem_model(list(A = list(indicators = c("x", "y"), scales = "ordinal"))), "cssem_model")
})
