#' Summarise DMFT results by group
#'
#' Takes the output of [calc_dmft()] and produces a summary table with
#' n, mean number of teeth, mean DT, mean DMFT, and optionally other
#' columns — ready to pass directly to [build_odontograph()] via `stats`
#' and `stats_raw`.
#'
#' @param dmft_data Data frame returned by [calc_dmft()], optionally merged
#'   with grouping variables (e.g. treatment arm, visit frequency).
#' @param group_col Column name to summarise by (e.g. `"treatment"`).
#' @param digits Number of decimal places for means (default 1).
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{stats}{Summary data frame with one row per group, containing
#'       `n`, `mean_teeth`, `mean_DT`, `mean_DMFT`. Column names match
#'       what [build_odontograph()] expects for `stats`.}
#'     \item{raw}{The input data frame, for passing to `stats_raw` in
#'       [build_odontograph()].}
#'   }
#' @export
#' @examples
#' data(sim_exam)
#' dmft <- calc_dmft(sim_exam)
#' dmft$treatment <- ifelse(
#'   as.numeric(gsub("P", "", dmft$record_id)) <= 10, "SDF", "ART"
#' )
#' result <- summarise_dmft(dmft, "treatment")
#' result$stats
#' # Pass directly to build_odontograph:
#' # build_odontograph(d, strata = "treatment",
#' #   stats = result$stats, stats_raw = result$raw,
#' #   stats_test = "wilcox", stats_var = "DMFT")
summarise_dmft <- function(dmft_data, group_col, digits = 1) {

  stats_df <- dmft_data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) |>
    dplyr::summarise(
      n          = dplyr::n(),
      mean_teeth = round(mean(.data$num_teeth, na.rm = TRUE), digits),
      mean_DT    = round(mean(.data$DT, na.rm = TRUE), digits),
      mean_DMFT  = round(mean(.data$DMFT, na.rm = TRUE), digits),
      .groups    = "drop"
    )

  list(stats = stats_df, raw = dmft_data)
}
