test_that("calc_dmft returns correct columns", {
  d <- expand.grid(
    record_id     = c("A", "B"),
    tooth_num     = paste0("ur", 1:3),
    tooth_surface = c("buc", "occ", "mes"),
    stringsAsFactors = FALSE
  )
  d$lesion_code  <- 0L
  d$act          <- 0L
  d$filling_code <- 0L
  d$code         <- 2L

  res <- calc_dmft(d)
  expect_true(all(c("record_id", "DT", "FT", "MT", "DFT", "DMFT",
                     "num_teeth", "DT_yn", "DFT_yn") %in% names(res)))
  expect_equal(nrow(res), 2L)
})

test_that("calc_dmft counts decayed teeth correctly", {
  # One patient, 2 teeth, 2 surfaces each
  d <- data.frame(
    record_id     = "P1",
    tooth_num     = rep(c("ur1", "ur2"), each = 2),
    tooth_surface = rep(c("buc", "occ"), 2),
    lesion_code   = c(4L, 0L, 0L, 0L),
    act           = c(2L, 0L, 0L, 0L),
    filling_code  = c(0L, 0L, 0L, 0L),
    code          = c(2L, 2L, 2L, 2L),
    stringsAsFactors = FALSE
  )

  res <- calc_dmft(d)
  expect_equal(res$DT, 1L)   # only ur1 has decay
  expect_equal(res$FT, 0L)
  expect_equal(res$MT, 0L)
  expect_equal(res$DMFT, 1L)
  expect_equal(res$num_teeth, 2L)
})

test_that("calc_dmft counts missing teeth correctly", {
  d <- data.frame(
    record_id     = "P1",
    tooth_num     = rep(c("ur1", "ur2"), each = 2),
    tooth_surface = rep(c("buc", "occ"), 2),
    lesion_code   = c(0L, 0L, 0L, 0L),
    act           = c(0L, 0L, 0L, 0L),
    filling_code  = c(0L, 0L, 0L, 0L),
    code          = c(1L, 1L, 2L, 2L),  # ur1 is missing
    stringsAsFactors = FALSE
  )

  res <- calc_dmft(d)
  expect_equal(res$MT, 1L)
  expect_equal(res$DMFT, 1L)
})

test_that("calc_dmft counts filled teeth correctly", {
  d <- data.frame(
    record_id     = "P1",
    tooth_num     = rep(c("ur1", "ur2"), each = 2),
    tooth_surface = rep(c("buc", "occ"), 2),
    lesion_code   = c(0L, 0L, 0L, 0L),
    act           = c(0L, 0L, 0L, 0L),
    filling_code  = c(1L, 0L, 0L, 0L),  # ur1-buc has filling
    code          = c(2L, 2L, 2L, 2L),
    stringsAsFactors = FALSE
  )

  res <- calc_dmft(d)
  expect_equal(res$FT, 1L)
  expect_equal(res$DFT, 1L)
})

test_that("calc_dmft handles grouping", {
  d <- data.frame(
    record_id          = "P1",
    redcap_event_name  = rep(c("week_0", "week_26"), each = 4),
    tooth_num          = rep(rep(c("ur1", "ur2"), each = 2), 2),
    tooth_surface      = rep(c("buc", "occ"), 4),
    lesion_code        = c(4L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    act                = c(2L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    filling_code       = rep(0L, 8),
    code               = rep(2L, 8),
    stringsAsFactors   = FALSE
  )

  res <- calc_dmft(d, group = "redcap_event_name")
  expect_equal(nrow(res), 2L)
  expect_equal(res$DT[res$redcap_event_name == "week_0"], 1L)
  expect_equal(res$DT[res$redcap_event_name == "week_26"], 0L)
})

test_that("calc_dmft handles strata", {
  d <- data.frame(
    record_id     = rep(c("P1", "P2"), each = 4),
    treatment     = rep(c("SDF", "ART"), each = 4),
    tooth_num     = rep(rep(c("ur1", "ur2"), each = 2), 2),
    tooth_surface = rep(c("buc", "occ"), 4),
    lesion_code   = rep(0L, 8),
    act           = rep(0L, 8),
    filling_code  = rep(0L, 8),
    code          = rep(2L, 8),
    stringsAsFactors = FALSE
  )

  res <- calc_dmft(d, strata = "treatment")
  expect_true("treatment" %in% names(res))
  expect_equal(nrow(res), 2L)
})

test_that("calc_dmft separates root caries", {
  d <- data.frame(
    record_id     = "P1",
    tooth_num     = rep("ur1", 4),
    tooth_surface = c("buc", "occ", "rootb", "rootl"),
    lesion_code   = c(0L, 0L, 4L, 0L),
    act           = c(0L, 0L, 2L, 0L),
    filling_code  = rep(0L, 4),
    code          = rep(2L, 4),
    stringsAsFactors = FALSE
  )

  res <- calc_dmft(d, root_lesion_col = "lesion_code")
  expect_equal(res$DT, 0L)   # coronal is clean
  expect_equal(res$RDT, 1L)  # root has decay
})

test_that("calc_dmft handles all-sound data", {
  d <- data.frame(
    record_id     = "P1",
    tooth_num     = rep("ur1", 2),
    tooth_surface = c("buc", "occ"),
    lesion_code   = c(0L, 0L),
    act           = c(0L, 0L),
    filling_code  = c(0L, 0L),
    code          = c(2L, 2L),
    stringsAsFactors = FALSE
  )

  res <- calc_dmft(d)
  expect_equal(res$DMFT, 0L)
  expect_equal(res$DT_yn, 0L)
})

test_that("calc_dmft consider_activity toggles the activity requirement", {
  d <- data.frame(
    record_id     = "P1",
    tooth_num     = rep("ur1", 2),
    tooth_surface = c("buc", "occ"),
    lesion_code   = c(5L, 0L),   # cavitated lesion on buccal surface
    act           = c(1L, 0L),   # but coded INACTIVE
    filling_code  = c(0L, 0L),
    code          = c(2L, 2L),
    stringsAsFactors = FALSE
  )

  # Default: activity required -> inactive cavitated lesion not counted
  expect_equal(calc_dmft(d)$DT, 0L)
  # Ignore activity -> cavitated lesion counted regardless of activity
  expect_equal(calc_dmft(d, consider_activity = FALSE)$DT, 1L)
})

test_that("calc_dmft consider_activity = FALSE works without an activity column", {
  d <- data.frame(
    record_id     = "P1",
    tooth_num     = rep("ur1", 2),
    tooth_surface = c("buc", "occ"),
    lesion_code   = c(5L, 0L),
    filling_code  = c(0L, 0L),
    code          = c(2L, 2L),
    stringsAsFactors = FALSE
  )
  expect_equal(calc_dmft(d, consider_activity = FALSE)$DT, 1L)
})

test_that("calc_dmft consider_activity_root overrides the coronal setting", {
  d <- data.frame(
    record_id     = "P1",
    tooth_num     = rep("ur1", 3),
    tooth_surface = c("buc", "rootb", "rootl"),
    lesion_code   = c(0L, 5L, 0L),  # cavitated root lesion
    act           = c(0L, 1L, 0L),  # coded inactive
    filling_code  = rep(0L, 3),
    code          = rep(2L, 3),
    stringsAsFactors = FALSE
  )
  # Default requires activity for roots too -> inactive root lesion not counted
  expect_equal(calc_dmft(d, root_lesion_col = "lesion_code")$RDT, 0L)
  # Turn activity off for roots only -> counted
  expect_equal(
    calc_dmft(d, root_lesion_col = "lesion_code",
              consider_activity_root = FALSE)$RDT,
    1L
  )
})
