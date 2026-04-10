test_that("pivot_to_long reshapes correctly", {
  wide <- data.frame(
    record_id    = "P1",
    ur1_buc_les  = 4L,
    ur1_buc_fil  = 0L,
    ur1_buc_act  = 2L,
    ur1_code     = 2L,
    stringsAsFactors = FALSE
  )

  long <- pivot_to_long(wide)
  expect_equal(nrow(long), 1L)
  expect_equal(long$tooth_num, "ur1")
  expect_equal(long$tooth_surface, "buc")
  expect_equal(long$lesion_code, 4L)
  expect_equal(long$filling_code, 0L)
  expect_equal(long$act, 2L)
  expect_equal(long$code, 2L)
})

test_that("pivot_to_long errors on bad suffixes", {
  wide <- data.frame(record_id = "P1", x = 1, stringsAsFactors = FALSE)
  expect_error(pivot_to_long(wide))
})
