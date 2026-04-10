test_that("calc_dmfs returns correct columns", {
  d <- data.frame(
    record_id     = "P1",
    tooth_num     = rep("ur1", 3),
    tooth_surface = c("buc", "occ", "mes"),
    lesion_code   = c(0L, 0L, 0L),
    act           = c(0L, 0L, 0L),
    filling_code  = c(0L, 0L, 0L),
    code          = c(2L, 2L, 2L),
    stringsAsFactors = FALSE
  )

  res <- calc_dmfs(d)
  expect_true(all(c("record_id", "DS", "FS", "MS", "DFS", "DMFS",
                     "DS_yn", "DFS_yn") %in% names(res)))
})

test_that("calc_dmfs counts surfaces correctly", {
  d <- data.frame(
    record_id     = "P1",
    tooth_num     = rep("ur1", 3),
    tooth_surface = c("buc", "occ", "mes"),
    lesion_code   = c(4L, 4L, 0L),
    act           = c(2L, 2L, 0L),
    filling_code  = c(0L, 0L, 1L),
    code          = c(2L, 2L, 2L),
    stringsAsFactors = FALSE
  )

  res <- calc_dmfs(d)
  expect_equal(res$DS, 2L)   # buc and occ
  expect_equal(res$FS, 1L)   # mes (not decayed, has filling)
  expect_equal(res$DFS, 3L)
})

test_that("calc_dmfs separates root surfaces", {
  d <- data.frame(
    record_id     = "P1",
    tooth_num     = rep("ur1", 4),
    tooth_surface = c("buc", "occ", "rootb", "rootl"),
    lesion_code   = c(0L, 0L, 5L, 0L),
    act           = c(0L, 0L, 2L, 0L),
    filling_code  = rep(0L, 4),
    code          = rep(2L, 4),
    stringsAsFactors = FALSE
  )

  res <- calc_dmfs(d, root_lesion_col = "lesion_code")
  expect_equal(res$DS, 0L)
  expect_equal(res$RDS, 1L)
})
