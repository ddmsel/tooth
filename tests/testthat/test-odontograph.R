test_that("draw_tooth returns a named list with correct elements", {
  sv <- data.frame(
    tooth_surface = c("buc", "lin", "mes", "dis", "occ"),
    prop = c(0.1, 0.2, 0.3, 0.4, 0.5),
    stringsAsFactors = FALSE
  )
  res <- draw_tooth("ur1", "ur", 1, TRUE, sv)
  expect_type(res, "list")
  expect_true(all(c("crown", "roots", "labels", "num_label",
                     "diags", "outline") %in% names(res)))
  expect_s3_class(res$crown, "data.frame")
})

test_that("draw_tooth respects surfaces argument", {
  sv <- data.frame(
    tooth_surface = c("buc", "occ"),
    prop = c(0.1, 0.5),
    stringsAsFactors = FALSE
  )
  res <- draw_tooth("ur1", "ur", 1, TRUE, sv,
                    surfaces = c("buc", "occ"), show_roots = FALSE)
  # Only buc and occ should appear in crown
  expect_true(all(unique(res$crown$surface) %in% c("buc", "occ")))
  expect_equal(nrow(res$roots), 0L)
})

test_that("build_odontograph returns a ggplot", {
  d <- expand.grid(
    tooth_num = paste0(rep(c("ur", "ul", "lr", "ll"), each = 5), 1:5),
    tooth_surface = c("buc", "lin", "mes", "dis", "occ"),
    stringsAsFactors = FALSE
  )
  d$prop <- runif(nrow(d))

  p <- build_odontograph(d, dentition = "primary",
                         surfaces = c("buc", "lin", "mes", "dis", "occ"),
                         show_roots = FALSE)
  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")
})

test_that("build_odontograph handles teeth_per_quadrant", {
  d <- expand.grid(
    tooth_num = paste0(rep(c("ur", "ul", "lr", "ll"), each = 7), 1:7),
    tooth_surface = c("buc", "occ"),
    stringsAsFactors = FALSE
  )
  d$prop <- runif(nrow(d))

  p <- build_odontograph(d, teeth_per_quadrant = 7,
                         surfaces = c("buc", "occ"), show_roots = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("build_odontograph strata returns combined plot", {
  d <- expand.grid(
    treatment = c("A", "B"),
    tooth_num = paste0(rep(c("ur", "ul", "lr", "ll"), each = 5), 1:5),
    tooth_surface = c("buc", "occ"),
    stringsAsFactors = FALSE
  )
  d$prop <- runif(nrow(d))

  p <- build_odontograph(d, dentition = "primary",
                         surfaces = c("buc", "occ"), show_roots = FALSE,
                         strata = "treatment")
  expect_s3_class(p, "gg")
})

test_that("build_odontograph strata with combine=FALSE returns list", {
  d <- expand.grid(
    treatment = c("A", "B"),
    tooth_num = paste0(rep(c("ur", "ul", "lr", "ll"), each = 5), 1:5),
    tooth_surface = c("buc", "occ"),
    stringsAsFactors = FALSE
  )
  d$prop <- runif(nrow(d))

  res <- build_odontograph(d, dentition = "primary",
                           surfaces = c("buc", "occ"), show_roots = FALSE,
                           strata = "treatment", combine = FALSE)
  expect_type(res, "list")
  expect_equal(length(res), 2L)
  expect_s3_class(res[[1]], "ggplot")
})
