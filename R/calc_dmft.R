#' Calculate DMFT / dmft index from surface- or tooth-level data
#'
#' Computes Decayed, Missing, and Filled Teeth counts per person.
#' Handles both primary and permanent dentition. Can separate root
#' caries from coronal caries. Accepts long or wide format data.
#'
#' @param data Data frame. In **long format**: one row per tooth-surface with
#'   columns for id, tooth_num, tooth_surface, and clinical codes. In **wide
#'   format**: one row per person-tooth with surface codes in separate columns.
#' @param format `"long"` (default) or `"wide"`. See Details.
#' @param id Person-identifier column (default `"record_id"`).
#' @param group Optional grouping column(s) for repeated measures, e.g.
#'   `"redcap_event_name"`. Character vector.
#' @param strata Optional stratification column(s), e.g. `"treatment"`. The
#'   output will include these columns for downstream group summaries.
#' @param lesion_col,activity_col,filling_col,tooth_code_col Column names for
#'   ICDAS codes (long format).
#' @param root_lesion_col Optional column for root surface lesion codes. When
#'   provided, root caries (RDT) is calculated separately from coronal (DT).
#'   Set to NULL to ignore root caries (default).
#' @param root_surfaces Character vector of surface names considered root
#'   surfaces. Default `c("rootb","rootl","rootm","rootd")`.
#' @param decayed_codes,activity_codes,filled_codes,present_codes,missing_codes
#'   Numeric vectors defining clinical code categories. Defaults match ICDAS.
#'   See the *Diagnostic threshold* section for `decayed_codes`.
#' @param root_decayed_codes Numeric vector for root caries codes (default
#'   same as `decayed_codes`).
#' @param consider_activity Logical; overall default for whether a lesion must
#'   also carry an active-caries code (`activity_codes`) to be counted as
#'   decayed. `TRUE` (default) requires activity; `FALSE` counts any lesion in
#'   `decayed_codes` regardless of activity. Standard epidemiological D(3)MFT
#'   counts all cavitated lesions irrespective of activity, so set `FALSE` to
#'   match that convention. When `FALSE`, the activity column is not referenced
#'   and need not be present.
#' @param consider_activity_coronal,consider_activity_root Optional logical
#'   overrides for the coronal and root components. `NULL` (default) inherits
#'   `consider_activity`. Use these to treat crown and root caries differently
#'   (e.g. require activity coronally but not for roots, where activity is often
#'   not assessed): `consider_activity_root = FALSE`.
#'
#' @section Diagnostic threshold:
#' The default `decayed_codes = c(3, 4, 5, 6)` implements the **D3 threshold**:
#' only ICDAS codes 3–6 (localised enamel breakdown through extensive
#' cavitation) are scored as decayed, while ICDAS 1–2 (non-cavitated initial
#' enamel lesions) are treated as sound. This D(3)MFT convention is the one
#' most commonly reported in caries epidemiology. To use the more sensitive D1
#' threshold that also counts early enamel lesions, pass
#' `decayed_codes = c(1, 2, 3, 4, 5, 6)`.
#'
#'
#' @section Wide format:
#' When `format = "wide"`, each row is one person (and optional group).
#' You must supply columns named like `{tooth}_{surface}_{code_type}` or use
#' the `wide_cols` parameter. Alternatively, pass a `pivot_spec` — but the
#' easiest approach is to reshape to long with [pivot_to_long()] first.
#'
#' @return A tibble with one row per person (and group/strata) containing:
#'   \describe{
#'     \item{DT}{Decayed teeth (coronal)}
#'     \item{FT}{Filled teeth (not decayed)}
#'     \item{MT}{Missing teeth}
#'     \item{DFT}{Decayed or filled teeth}
#'     \item{DMFT}{Decayed + missing + filled teeth}
#'     \item{num_teeth}{Count of teeth present}
#'     \item{RDT}{Root-decayed teeth (only when `root_lesion_col` is set)}
#'     \item{DT_yn, DFT_yn}{Binary indicators}
#'   }
#' @export
calc_dmft <- function(data,
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
                      present_codes  = c(2, 3, 4, 5, 6, 7, 8),
                      missing_codes  = c(1),
                      root_decayed_codes = NULL,
                      consider_activity         = TRUE,
                      consider_activity_coronal = NULL,
                      consider_activity_root    = NULL) {

  format <- match.arg(format)
  if (is.null(root_decayed_codes)) root_decayed_codes <- decayed_codes

  # Resolve per-component activity handling. Component-specific arguments
  # override the overall `consider_activity` default; NULL means "inherit".
  ca_coronal <- if (is.null(consider_activity_coronal))
    consider_activity else consider_activity_coronal
  ca_root <- if (is.null(consider_activity_root))
    consider_activity else consider_activity_root

  # --- Wide → long conversion ---
  if (format == "wide") {
    data <- pivot_to_long(data, id = id, group = group, strata = strata)
  }

  # Separate root vs coronal
  is_root <- data$tooth_surface %in% root_surfaces
  coronal <- data[!is_root, ]
  root    <- if (!is.null(root_lesion_col)) data[is_root, ] else NULL

  grp_tooth   <- c(id, group, strata, "tooth_num")
  grp_person  <- c(id, group, strata)

  # ---- Coronal: surface → tooth ----
  tooth_level <- coronal |>
    dplyr::mutate(
      .ds = ifelse(.data[[lesion_col]] %in% decayed_codes &
                     (if (ca_coronal)
                        .data[[activity_col]] %in% activity_codes else TRUE),
                   1, 0),
      .fs = ifelse(.data$.ds < 1 & .data[[filling_col]] %in% filled_codes, 1, 0),
      .ms = ifelse(.data[[tooth_code_col]] %in% missing_codes, 1, 0),
      .present = ifelse(.data[[tooth_code_col]] %in% present_codes, 1, 0)
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_tooth))) |>
    dplyr::summarise(
      DS = sum(.data$.ds, na.rm=TRUE),
      FS = sum(.data$.fs, na.rm=TRUE),
      MS = sum(.data$.ms, na.rm=TRUE),
      present = max(.data$.present, na.rm=TRUE),
      .groups = "drop"
    )

  # ---- Coronal: tooth → person ----
  result <- tooth_level |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_person))) |>
    dplyr::summarise(
      DT        = sum(.data$DS > 0, na.rm=TRUE),
      FT        = sum(.data$DS < 1 & .data$FS > 0, na.rm=TRUE),
      MT        = sum(.data$MS > 0 & .data$DS < 1 & .data$FS < 1, na.rm=TRUE),
      DFT       = sum(.data$DS > 0 | .data$FS > 0, na.rm=TRUE),
      DMFT      = sum(.data$DS > 0 | .data$FS > 0 |
                        (.data$MS > 0 & .data$DS < 1 & .data$FS < 1), na.rm=TRUE),
      num_teeth = sum(.data$present > 0, na.rm=TRUE),
      .groups   = "drop"
    ) |>
    dplyr::mutate(
      DT_yn  = ifelse(.data$DT  >= 1, 1L, 0L),
      DFT_yn = ifelse(.data$DFT >= 1, 1L, 0L)
    )

  # ---- Root caries (optional) ----
  if (!is.null(root_lesion_col) && !is.null(root)) {
    lcol <- if (root_lesion_col %in% names(root)) root_lesion_col else lesion_col

    root_tooth <- root |>
      dplyr::mutate(
        .rds = ifelse(.data[[lcol]] %in% root_decayed_codes &
                        (if (ca_root)
                           .data[[activity_col]] %in% activity_codes else TRUE),
                      1, 0)
      ) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_tooth))) |>
      dplyr::summarise(RDS = sum(.data$.rds, na.rm=TRUE), .groups="drop")

    root_person <- root_tooth |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_person))) |>
      dplyr::summarise(RDT = sum(.data$RDS > 0, na.rm=TRUE), .groups="drop")

    result <- dplyr::left_join(result, root_person, by = grp_person) |>
      dplyr::mutate(RDT = ifelse(is.na(.data$RDT), 0L, .data$RDT))
  }

  result
}


#' Pivot wide tooth data to long format
#'
#' Helper to reshape wide-format data (one row per person-tooth, surfaces
#' as separate columns) into the long format expected by [calc_dmft()].
#'
#' Expects columns following the naming convention:
#' `{tooth}_{surface}_les`, `{tooth}_{surface}_fil`, `{tooth}_{surface}_act`,
#' and `{tooth}_code` for tooth-level status.
#'
#' @param data Wide data frame.
#' @param id Person ID column name.
#' @param group Optional grouping columns.
#' @param strata Optional stratification columns.
#' @param lesion_suffix,filling_suffix,activity_suffix Suffixes that
#'   identify the surface-level columns (default `"_les"`, `"_fil"`, `"_act"`).
#' @param code_suffix Suffix for tooth-level code columns (default `"_code"`).
#'
#' @return A long-format tibble.
#' @export
pivot_to_long <- function(data, id = "record_id", group = NULL, strata = NULL,
                          lesion_suffix = "_les", filling_suffix = "_fil",
                          activity_suffix = "_act", code_suffix = "_code") {

  keep_cols <- c(id, group, strata)
  meta <- data[, keep_cols, drop = FALSE]

  # Find lesion columns → extract tooth_num and surface
  les_cols <- grep(paste0(lesion_suffix, "$"), names(data), value = TRUE)
  if (length(les_cols) == 0) {
    stop("No columns matching lesion suffix '", lesion_suffix,
         "' found. Check your column naming convention.")
  }

  # Parse: remove suffix, split last segment as surface, rest as tooth
  parsed <- strsplit(sub(paste0(lesion_suffix, "$"), "", les_cols), "_")
  tooth_nums <- vapply(parsed, function(x) paste(x[-length(x)], collapse="_"), character(1))
  surfaces   <- vapply(parsed, function(x) x[length(x)], character(1))

  rows <- list()
  for (i in seq_along(les_cols)) {
    tn <- tooth_nums[i]; sf <- surfaces[i]
    base <- paste0(tn, "_", sf)
    code_col_name <- paste0(tn, code_suffix)

    row <- meta
    row$tooth_num     <- tn
    row$tooth_surface <- sf
    row$lesion_code   <- data[[paste0(base, lesion_suffix)]]
    fil_name <- paste0(base, filling_suffix)
    act_name <- paste0(base, activity_suffix)
    row$filling_code  <- if (fil_name %in% names(data)) data[[fil_name]] else NA_real_
    row$act           <- if (act_name %in% names(data)) data[[act_name]] else NA_real_
    row$code          <- if (code_col_name %in% names(data)) data[[code_col_name]] else NA_real_
    rows[[i]] <- row
  }

  dplyr::bind_rows(rows)
}
