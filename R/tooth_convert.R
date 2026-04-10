#' Convert between tooth numbering systems
#'
#' Converts between FDI (ISO 3950), Universal (ADA), and the internal
#' quadrant format used by this package (`"ur1"`, `"ll5"`, etc.).
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
#' tooth_convert(c("1","16","17","32"), from = "universal", to = "fdi")
tooth_convert <- function(x, from = "fdi", to = "quadrant",
                          dentition = "permanent") {

  # Build lookup: quadrant <-> FDI <-> Universal (permanent)
  quads  <- c("ur", "ul", "ll", "lr")
  fdi_q  <- c(1, 2, 3, 4)
  uni_starts <- c(1, 9, 24, 17)  # Universal tooth 1=UR8, 9=UL1, etc.

  tpq <- if (dentition == "primary") 5L else 8L

  lookup <- do.call(rbind, lapply(seq_along(quads), function(qi) {
    nums <- seq_len(tpq)
    fdi_nums <- if (dentition == "primary") nums + 0L else nums
    fdi_prefix <- if (dentition == "primary") fdi_q[qi] + 4L else fdi_q[qi]

    # Universal numbering
    if (qi == 1) { # UR: 1..8 (8 down to 1 positionally, but tooth 1=UR8)
      uni <- if (dentition == "permanent") rev(seq(1, tpq)) else rev(seq(51, 50+tpq))
    } else if (qi == 2) { # UL: 9..16
      uni <- if (dentition == "permanent") seq(tpq+1, 2*tpq) else seq(50+tpq+1, 50+2*tpq)
    } else if (qi == 3) { # LL: 24..17
      uni <- if (dentition == "permanent") rev(seq(2*tpq+1, 3*tpq)) else rev(seq(50+2*tpq+1, 50+3*tpq))
    } else { # LR: 25..32
      uni <- if (dentition == "permanent") seq(3*tpq+1, 4*tpq) else seq(50+3*tpq+1, 50+4*tpq)
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
