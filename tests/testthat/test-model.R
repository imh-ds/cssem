test_that("model validates declaration", {
  expect_error(cssem_model(list(A = list(indicators = "x", scales = "ordinal"))))
  expect_error(cssem_model(list(A = list(indicators = c("x", "y"), scales = "bad"))))
  expect_s3_class(cssem_model(list(A = list(indicators = c("x", "y"), scales = "ordinal"))), "cssem_model")
})

test_that("ordinal()/continuous() bundle indicators, scale, and default keys", {
  spec <- ordinal("x", "y", "z")
  expect_equal(spec$indicators, c("x", "y", "z"))
  expect_equal(spec$scales, "ordinal")
  expect_null(spec$keys)
  expect_equal(continuous("x", "y")$scales, "continuous")
  expect_equal(ordinal(paste0("x", 1:3))$indicators, c("x1", "x2", "x3"))
  expect_equal(ordinal("x", "y", keys = c(1, -1))$keys, c(1, -1))
  expect_error(ordinal())
})

test_that("specify_measurement() produces the same object as cssem_model()", {
  via_list <- cssem_model(list(
    Trust = list(indicators = c("trust_1", "trust_2"), scales = "ordinal"),
    Loyalty = list(indicators = c("loy_1", "loy_2", "loy_3"), scales = "continuous", keys = c(1, -1, 1))
  ), folds = 4)
  via_helpers <- specify_measurement(
    Trust = ordinal("trust_1", "trust_2"),
    Loyalty = continuous("loy_1", "loy_2", "loy_3", keys = c(1, -1, 1)),
    folds = 4
  )
  expect_equal(via_list, via_helpers)
})

test_that("specify_measurement() requires named ordinal()/continuous() declarations", {
  expect_error(specify_measurement(ordinal("x", "y")))
  expect_error(specify_measurement(Trust = c("x", "y")))
})

test_that("specify_measurement() exploratory preset lightens folds like cssem_model()", {
  expect_equal(
    cssem_model(list(A = list(indicators = c("x", "y"), scales = "ordinal")), preset = "exploratory")$folds,
    specify_measurement(A = ordinal("x", "y"), preset = "exploratory")$folds
  )
})
