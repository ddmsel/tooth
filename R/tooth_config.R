#' Generate tooth configuration for an arch
#'
#' Creates a tibble of tooth IDs for primary or permanent dentition
#' with a configurable number of teeth per quadrant (5-8).
#'
#' @param dentition Character: `"permanent"` (default) or `"primary"`.
#' @param teeth_per_quadrant Integer 5-8. Defaults to 8 for permanent, 5 for primary.
#'
#' @return A tibble with columns: `quadrant`, `num`, `tooth_id`, `is_upper`.
#' @export
tooth_config <- function(dentition = c("permanent", "primary"),
                         teeth_per_quadrant = NULL) {
  dentition <- match.arg(dentition)

  if (is.null(teeth_per_quadrant)) {
    teeth_per_quadrant <- switch(dentition, permanent = 8L, primary = 5L)
  }
  stopifnot(teeth_per_quadrant >= 5, teeth_per_quadrant <= 8)
  teeth_per_quadrant <- as.integer(teeth_per_quadrant)

  quads <- c("ur", "ul", "lr", "ll")
  tibble::tibble(
    quadrant = rep(quads, each = teeth_per_quadrant),
    num      = rep(seq_len(teeth_per_quadrant), 4)
  ) |>
    dplyr::mutate(
      tooth_id = paste0(.data$quadrant, .data$num),
      is_upper = .data$quadrant %in% c("ur", "ul")
    )
}
