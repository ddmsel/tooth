test_that("tooth_config returns correct number of teeth", {
  cfg <- tooth_config("permanent")
  expect_equal(nrow(cfg), 32L)

  cfg5 <- tooth_config("primary")
  expect_equal(nrow(cfg5), 20L)

  cfg7 <- tooth_config("permanent", teeth_per_quadrant = 7)
  expect_equal(nrow(cfg7), 28L)
})

test_that("tooth_config has correct columns", {
  cfg <- tooth_config()
  expect_true(all(c("quadrant", "num", "tooth_id", "is_upper") %in% names(cfg)))
})

test_that("tooth_config errors on invalid teeth_per_quadrant", {
  expect_error(tooth_config(teeth_per_quadrant = 4))
  expect_error(tooth_config(teeth_per_quadrant = 9))
})

test_that("tooth_config upper/lower flags are correct", {
  cfg <- tooth_config("permanent", 7)
  expect_true(all(cfg$is_upper[cfg$quadrant %in% c("ur", "ul")]))
  expect_true(all(!cfg$is_upper[cfg$quadrant %in% c("lr", "ll")]))
})
