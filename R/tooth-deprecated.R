#' Deprecated functions in tooth
#'
#' These functions are kept for backward compatibility but will be removed
#' in a future release. Each forwards to its renamed replacement.
#'
#' @details
#' \describe{
#'   \item{`build_odontograph()`}{Deprecated in 0.5.0. The visualization is
#'     now consistently called an *odontogram* throughout the package. Use
#'     [build_odontogram()] instead. This wrapper forwards all arguments and
#'     is fully equivalent, but emits a deprecation warning.}
#' }
#'
#' @name tooth-deprecated
#' @keywords internal
NULL

#' @rdname tooth-deprecated
#' @param ... Arguments passed on to [build_odontogram()].
#' @return `build_odontograph()` returns the value of [build_odontogram()]
#'   (a `ggplot` object, or a list of `ggplot`s when `combine = FALSE`).
#' @export
#' @examples
#' library(tibble)
#' d <- expand.grid(
#'   tooth_num = paste0(rep(c("ur", "ul", "lr", "ll"), each = 7), 1:7),
#'   tooth_surface = c("buc", "lin", "mes", "dis", "occ"),
#'   stringsAsFactors = FALSE
#' )
#' d$prop <- runif(nrow(d))
#' # Old name still works, with a warning:
#' \dontrun{
#' build_odontograph(d, teeth_per_quadrant = 7)
#' }
#' # Preferred:
#' build_odontogram(d, teeth_per_quadrant = 7)
build_odontograph <- function(...) {
  .Deprecated(
    new = "build_odontogram",
    package = "tooth",
    msg = paste0(
      "'build_odontograph()' is deprecated as of tooth 0.5.0.\n",
      "Use 'build_odontogram()' instead."
    )
  )
  build_odontogram(...)
}
