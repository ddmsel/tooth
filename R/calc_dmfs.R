#' Calculate DMFS / dmfs index from surface-level data
#'
#' Computes Decayed, Missing, and Filled Surfaces per person.
#' Can separate root from coronal surfaces. Accepts long or wide format.
#'
#' @inheritParams calc_dmft
#' @param decayed_codes Numeric vector of lesion codes considered **decayed**
#'   (default `c(3,4,5,6)` for ICDAS).
#' @param activity_codes Numeric vector of activity codes indicating **active**
#'   caries (default `c(2)`).
#' @param filled_codes Numeric vector of filling codes indicating a
#'   **restoration** is present (default `c(1,2,4,5,6,7,8)`).
#' @param missing_codes Numeric vector of tooth-level codes indicating the
#'   tooth is **missing** (default `c(1)`).
#' @return A tibble with one row per person (and group/strata) containing:
#'   `DS`, `FS`, `MS`, `DFS`, `DMFS`, plus optional `RDS` (root decayed
#'   surfaces) when `root_lesion_col` is set, and binary indicators.
#' @export
calc_dmfs <- function(data,
                      format         = c("long", "wide"),
                      id             = "record_id",
                      group          = NULL,
                      strata         = NULL,
                      lesion_col     = "lesion_code",
                      activity_col   = "act",
                      filling_col    = "filling_code",
                      tooth_code_col = "code",
                      root_lesion_col = NULL,
                      root_surfaces  = c("rootb","rootl","rootm","rootd"),
                      decayed_codes  = c(3, 4, 5, 6),
                      activity_codes = c(2),
                      filled_codes   = c(1, 2, 4, 5, 6, 7, 8),
                      missing_codes  = c(1),
                      root_decayed_codes = NULL) {

  format <- match.arg(format)
  if (is.null(root_decayed_codes)) root_decayed_codes <- decayed_codes

  if (format == "wide") {
    data <- pivot_to_long(data, id = id, group = group, strata = strata)
  }

  is_root  <- data$tooth_surface %in% root_surfaces
  coronal  <- data[!is_root, ]
  root     <- if (!is.null(root_lesion_col)) data[is_root, ] else NULL

  grp <- c(id, group, strata)

  # ---- Coronal surfaces ----
  result <- coronal |>
    dplyr::mutate(
      .ds = ifelse(.data[[lesion_col]] %in% decayed_codes &
                     .data[[activity_col]] %in% activity_codes, 1, 0),
      .fs = ifelse(.data$.ds < 1 & .data[[filling_col]] %in% filled_codes, 1, 0),
      .ms = ifelse(.data[[tooth_code_col]] %in% missing_codes, 1, 0)
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
    dplyr::summarise(
      DS   = sum(.data$.ds, na.rm = TRUE),
      FS   = sum(.data$.ds < 1 & .data$.fs > 0, na.rm = TRUE),
      MS   = sum(.data$.ms, na.rm = TRUE),
      DFS  = sum(.data$.ds > 0 | .data$.fs > 0, na.rm = TRUE),
      DMFS = sum(.data$.ds > 0 | .data$.fs > 0 | .data$.ms > 0, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      DS_yn  = ifelse(.data$DS  >= 1, 1L, 0L),
      DFS_yn = ifelse(.data$DFS >= 1, 1L, 0L)
    )

  # ---- Root surfaces (optional) ----
  if (!is.null(root_lesion_col) && !is.null(root)) {
    lcol <- if (root_lesion_col %in% names(root)) root_lesion_col else lesion_col

    root_result <- root |>
      dplyr::mutate(.rds = ifelse(.data[[lcol]] %in% root_decayed_codes, 1, 0)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
      dplyr::summarise(RDS = sum(.data$.rds, na.rm = TRUE), .groups = "drop")

    result <- dplyr::left_join(result, root_result, by = grp) |>
      dplyr::mutate(RDS = ifelse(is.na(.data$RDS), 0L, .data$RDS))
  }

  result
}
