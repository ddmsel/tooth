test_that("FDI to quadrant works", {
  expect_equal(tooth_convert("11", from = "fdi", to = "quadrant"), "ur1")
  expect_equal(tooth_convert("21", from = "fdi", to = "quadrant"), "ul1")
  expect_equal(tooth_convert("31", from = "fdi", to = "quadrant"), "ll1")
  expect_equal(tooth_convert("41", from = "fdi", to = "quadrant"), "lr1")
})

test_that("quadrant to FDI works", {
  expect_equal(tooth_convert("ur1", from = "quadrant", to = "fdi"), "11")
  expect_equal(tooth_convert("ll7", from = "quadrant", to = "fdi"), "37")
})

test_that("universal to FDI round-trips", {
  fdi_vals <- c("11", "18", "21", "28", "31", "38", "41", "48")
  uni <- tooth_convert(fdi_vals, from = "fdi", to = "universal")
  back <- tooth_convert(uni, from = "universal", to = "fdi")
  expect_equal(back, fdi_vals)
})

test_that("unknown teeth return NA", {
  expect_true(is.na(tooth_convert("99", from = "fdi", to = "quadrant")))
})

test_that("vectorised input works", {
  res <- tooth_convert(c("11", "21"), from = "fdi", to = "quadrant")
  expect_equal(res, c("ur1", "ul1"))
})
