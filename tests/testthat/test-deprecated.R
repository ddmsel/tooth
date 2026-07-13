test_that("build_odontograph is deprecated but still works", {
  d <- expand.grid(
    tooth_num     = paste0("ur", 1:7),
    tooth_surface = c("buc", "lin", "mes", "dis", "occ"),
    stringsAsFactors = FALSE
  )
  d$prop <- 0.5

  # Emits a deprecation warning
  expect_warning(
    build_odontograph(d, teeth_per_quadrant = 7),
    "deprecated"
  )

  # Forwards to build_odontogram and returns the same class of object
  p_old <- suppressWarnings(build_odontograph(d, teeth_per_quadrant = 7))
  p_new <- build_odontogram(d, teeth_per_quadrant = 7)
  expect_s3_class(p_old, "ggplot")
  expect_identical(class(p_old), class(p_new))
})
