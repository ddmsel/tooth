#' Convert between tooth numbering systems
#'
#' Converts between FDI (ISO 3950), Universal (ADA), and the internal
#' quadrant format used by this package (`"ur1"`, `"ll5"`, etc.).
#'
#' @details
#' The `"universal"` system follows the standard ADA conventions:
#' permanent teeth are numbered **1–32** (1 = upper-right third molar,
#' 32 = lower-right third molar), while primary teeth use the **letters
#' A–T** (A = upper-right second primary molar, T = lower-right second
#' primary molar). FDI two-digit codes are `11`–`48` for permanent and
#' `51`–`85` for primary dentition.
#'
#' @param x Character or numeric vector of tooth numbers.
#' @param from Numbering system of the input: `"fdi"`, `"universal"`, or
#'   `"quadrant"`.
#' @param to Target numbering system: `"fdi"`, `"universal"`, or `"quadrant"`.
#' @param dentition `"permanent"` or `"primary"`. Required when converting
#'   from/to `"universal"` with primary teeth.
#'
#' @return Character vector in the target system.
#' @export
#' @examples
#' tooth_convert("11", from = "fdi", to = "quadrant")
#' tooth_convert("ur1", from = "quadrant", to = "fdi")
#' tooth_convert(c("1", "16", "17", "32"), from = "universal", to = "fdi")
#' # Primary dentition uses ADA letters A-T for the universal system:
#' tooth_convert("ur1", from = "quadrant", to = "universal",
#'               dentition = "primary")   # "E"
#' tooth_convert("K", from = "universal", to = "fdi", dentition = "primary")
tooth_convert <- function(x, from = "fdi", to = "quadrant",
                          dentition = "permanent") {

  # Build lookup: quadrant <-> FDI <-> Universal
  quads  <- c("ur", "ul", "ll", "lr")
  fdi_q  <- c(1, 2, 3, 4)

  tpq <- if (dentition == "primary") 5L else 8L

  lookup <- do.call(rbind, lapply(seq_along(quads), function(qi) {
    nums <- seq_len(tpq)
    fdi_nums <- if (dentition == "primary") nums + 0L else nums
    fdi_prefix <- if (dentition == "primary") fdi_q[qi] + 4L else fdi_q[qi]

    # Universal numbering.
    #   Permanent: numeric 1-32 (1 = UR8 ... 32 = LR8).
    #   Primary:   ADA letters A-T (A = UR 2nd molar ... T = LR 2nd molar).
    if (qi == 1) {          # Upper right (tooth 1 = central incisor)
      uni <- if (dentition == "permanent") rev(seq(1, tpq))
             else rev(LETTERS[seq_len(tpq)])                    # E D C B A
    } else if (qi == 2) {   # Upper left
      uni <- if (dentition == "permanent") seq(tpq + 1, 2 * tpq)
             else LETTERS[seq(tpq + 1, 2 * tpq)]                # F G H I J
    } else if (qi == 3) {   # Lower left
      uni <- if (dentition == "permanent") rev(seq(2 * tpq + 1, 3 * tpq))
             else rev(LETTERS[seq(2 * tpq + 1, 3 * tpq)])       # O N M L K
    } else {                # Lower right
      uni <- if (dentition == "permanent") seq(3 * tpq + 1, 4 * tpq)
             else LETTERS[seq(3 * tpq + 1, 4 * tpq)]            # P Q R S T
    }

    data.frame(
      quadrant = paste0(quads[qi], nums),
      fdi      = paste0(fdi_prefix, fdi_nums),
      universal = as.character(uni),
      stringsAsFactors = FALSE
    )
  }))

  x_char <- as.character(x)

  from_col <- switch(from,
                     fdi = "fdi", universal = "universal", quadrant = "quadrant",
                     stop("Unknown 'from': ", from))
  to_col <- switch(to,
                   fdi = "fdi", universal = "universal", quadrant = "quadrant",
                   stop("Unknown 'to': ", to))

  idx <- match(x_char, lookup[[from_col]])
  result <- lookup[[to_col]][idx]
  result
}
