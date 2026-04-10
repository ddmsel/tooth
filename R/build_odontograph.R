#' Build a surface-level odontograph heatmap
#'
#' Draws a full-arch odontograph with colour-coded tooth surfaces.
#' Supports primary and permanent dentition (5–8 teeth per quadrant),
#' selective surfaces, and stratification by any grouping variable.
#'
#' @param data Data frame with at least `tooth_num` (e.g. `"ur1"`),
#'   `tooth_surface` (e.g. `"buc"`, `"occ"`, `"rootb"`), and a numeric value
#'   column. If stratifying, include the column named in `strata`.
#' @param value_col Name of the numeric column to map to fill colour.
#' @param dentition `"permanent"` or `"primary"`.
#' @param teeth_per_quadrant Integer 5–8 (default derived from `dentition`).
#' @param title,subtitle Plot title/subtitle.
#' @param color_low,color_high Gradient endpoints.
#' @param na_color Fill for missing surfaces.
#' @param max_val Upper limit for the colour scale (auto-detected if NULL).
#' @param legend_title Legend title.
#' @param show_roots Logical; draw root caries bars.
#' @param surfaces Character vector of surfaces to draw. Default all 9:
#'   `c("buc","lin","mes","dis","occ","rootb","rootl","rootm","rootd")`.
#'   Use `c("buc","lin","mes","dis","occ")` for coronal-only, or
#'   `c("rootb","rootl","rootm","rootd")` for root-only.
#' @param show_labels Logical; show surface abbreviation labels.
#' @param label_size Numeric; size of surface labels.
#' @param strata Optional column name for stratification. When provided,
#'   returns a list of ggplots (one per stratum) or a combined plot using
#'   [patchwork::wrap_plots()] depending on `combine`.
#' @param combine Logical; if `strata` is set, combine panels into one plot
#'   (default TRUE) or return a named list.
#' @param ncol Number of columns when combining stratified panels.
#'
#' @return A `ggplot` object (or list of ggplots if `combine = FALSE`).
#' @export
#' @examples
#' library(tibble)
#' d <- expand.grid(
#'   tooth_num = paste0(rep(c("ur","ul","lr","ll"), each=7), 1:7),
#'   tooth_surface = c("buc","lin","mes","dis","occ"),
#'   stringsAsFactors = FALSE
#' )
#' d$prop <- runif(nrow(d))
#' build_odontograph(d, teeth_per_quadrant = 7)
build_odontograph <- function(data, value_col = "prop",
                              dentition = "permanent",
                              teeth_per_quadrant = NULL,
                              title = "Surface Odontograph",
                              subtitle = NULL,
                              color_low = "white", color_high = "red",
                              na_color = "grey90", max_val = NULL,
                              legend_title = "Proportion",
                              show_roots = TRUE,
                              surfaces = c("buc","lin","mes","dis","occ",
                                           "rootb","rootl","rootm","rootd"),
                              show_labels = TRUE,
                              label_size = 1.8,
                              strata = NULL,
                              combine = TRUE,
                              ncol = 1) {

  # --- Handle stratification ---
  if (!is.null(strata)) {
    strata_vals <- unique(data[[strata]])
    plots <- lapply(strata_vals, function(sv) {
      sub_data <- data[data[[strata]] == sv, ]
      build_odontograph(
        data = sub_data, value_col = value_col,
        dentition = dentition, teeth_per_quadrant = teeth_per_quadrant,
        title = paste0(title, " \u2014 ", sv), subtitle = subtitle,
        color_low = color_low, color_high = color_high,
        na_color = na_color, max_val = max_val,
        legend_title = legend_title, show_roots = show_roots,
        surfaces = surfaces, show_labels = show_labels,
        label_size = label_size, strata = NULL
      )
    })
    names(plots) <- as.character(strata_vals)

    if (combine) {
      return(patchwork::wrap_plots(plots, ncol = ncol))
    }
    return(plots)
  }

  # --- Determine root display from surfaces arg ---
  has_roots <- any(c("rootb","rootl","rootm","rootd") %in% surfaces)
  show_roots <- show_roots & has_roots

  cfg <- tooth_config(dentition, teeth_per_quadrant)
  tpq <- max(cfg$num)

  tooth_size   <- 1
  gap          <- 0.3
  midline_gap  <- 0.6
  row_gap      <- 0.8

  upper_right <- tibble::tibble(quadrant="ur", num=tpq:1,
                                 x_pos=(seq_len(tpq)-1)*(tooth_size+gap))
  upper_left  <- tibble::tibble(quadrant="ul", num=1:tpq,
                                 x_pos=max(upper_right$x_pos)+tooth_size+midline_gap+
                                   (seq_len(tpq)-1)*(tooth_size+gap))
  lower_right <- tibble::tibble(quadrant="lr", num=tpq:1, x_pos=upper_right$x_pos)
  lower_left  <- tibble::tibble(quadrant="ll", num=1:tpq, x_pos=upper_left$x_pos)

  positions <- dplyr::bind_rows(upper_right, upper_left, lower_right, lower_left) |>
    dplyr::mutate(
      tooth_id = paste0(.data$quadrant, .data$num),
      is_upper = .data$quadrant %in% c("ur", "ul"),
      y_pos    = ifelse(.data$is_upper, row_gap, -tooth_size - row_gap)
    )

  all <- list(crown=list(), roots=list(), labels=list(),
              nums=list(), diags=list(), outlines=list())

  for (i in seq_len(nrow(positions))) {
    pos <- positions[i, ]
    surf_data <- data[data$tooth_num == pos$tooth_id, ]
    parts <- draw_tooth(
      tooth_id=pos$tooth_id, quadrant=pos$quadrant,
      tooth_num=pos$num, is_upper=pos$is_upper,
      surface_values=surf_data, value_col=value_col,
      x_offset=pos$x_pos, y_offset=pos$y_pos,
      tooth_size=tooth_size, show_roots=show_roots,
      surfaces=surfaces
    )
    all$crown[[i]]    <- parts$crown
    all$roots[[i]]    <- parts$roots
    all$labels[[i]]   <- parts$labels
    all$nums[[i]]     <- parts$num_label
    all$diags[[i]]    <- parts$diags
    all$outlines[[i]] <- parts$outline
  }

  crown_df    <- dplyr::bind_rows(all$crown)
  roots_df    <- dplyr::bind_rows(all$roots)
  labels_df   <- dplyr::bind_rows(all$labels)
  nums_df     <- dplyr::bind_rows(all$nums)
  diags_df    <- dplyr::bind_rows(all$diags)
  outlines_df <- dplyr::bind_rows(all$outlines)

  if (is.null(max_val)) {
    all_vals <- c(crown_df$value, roots_df$value)
    max_val <- if (length(all_vals) > 0) max(all_vals, na.rm=TRUE) else 1
    if (is.infinite(max_val) || is.na(max_val)) max_val <- 1
  }

  midline_x <- max(upper_right$x_pos) + tooth_size + midline_gap/2
  upper_y   <- row_gap + tooth_size/2
  lower_y   <- -tooth_size - row_gap + tooth_size/2
  label_x   <- -0.8

  p <- ggplot2::ggplot()

  if (nrow(crown_df) > 0) {
    p <- p + ggplot2::geom_polygon(data=crown_df,
      ggplot2::aes(x=.data$x, y=.data$y, group=.data$group_id, fill=.data$value),
      color="grey50", linewidth=0.3)
  }
  if (nrow(diags_df) > 0 && nrow(crown_df) > 0) {
    p <- p + ggplot2::geom_segment(data=diags_df,
      ggplot2::aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend),
      color="grey50", linewidth=0.3)
  }
  if (nrow(outlines_df) > 0 && nrow(crown_df) > 0) {
    p <- p + ggplot2::geom_path(data=outlines_df,
      ggplot2::aes(x=.data$x, y=.data$y, group=.data$tooth),
      color="grey40", linewidth=0.5)
  }

  if (nrow(roots_df) > 0) {
    p <- p + ggplot2::geom_polygon(data=roots_df,
      ggplot2::aes(x=.data$x, y=.data$y, group=.data$group_id, fill=.data$value),
      color="grey50", linewidth=0.3)
  }
  if (show_labels && nrow(labels_df) > 0) {
    p <- p + ggplot2::geom_text(data=labels_df,
      ggplot2::aes(x=.data$x, y=.data$y, label=.data$label),
      size=label_size, color="grey40", fontface="bold")
  }

  side_label_y <- if (nrow(outlines_df) > 0) {
    max(outlines_df$y) + 0.6
  } else if (nrow(roots_df) > 0) {
    max(roots_df$y) + 0.6
  } else {
    row_gap + tooth_size + 0.6
  }

  p <- p +
    ggplot2::geom_text(data=nums_df,
      ggplot2::aes(x=.data$x, y=.data$y, label=toupper(.data$label)),
      size=2, color="grey30", fontface="bold") +
    ggplot2::geom_vline(xintercept=midline_x, linetype="dashed",
                        color="grey60", linewidth=0.4) +
    ggplot2::geom_hline(yintercept=0, color="black", linewidth=0.8) +
    ggplot2::annotate("text", x=label_x, y=upper_y, label="Upper",
                      fontface="bold", size=3.5, color="#6B4E97", hjust=1) +
    ggplot2::annotate("text", x=label_x, y=lower_y, label="Lower",
                      fontface="bold", size=3.5, color="#5A8D49", hjust=1) +
    ggplot2::annotate("text",
      x=(min(upper_right$x_pos)+max(upper_right$x_pos)+tooth_size)/2,
      y=side_label_y, label="Right", size=2.5, color="grey50") +
    ggplot2::annotate("text",
      x=(min(upper_left$x_pos)+max(upper_left$x_pos)+tooth_size)/2,
      y=side_label_y, label="Left", size=2.5, color="grey50") +
    ggplot2::scale_fill_gradient(low=color_low, high=color_high,
                                 na.value=na_color, limits=c(0, max_val),
                                 name=legend_title) +
    ggplot2::coord_fixed() +
    ggplot2::labs(title=title, subtitle=subtitle) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title=ggplot2::element_text(face="bold", size=12, hjust=0.5),
      plot.subtitle=ggplot2::element_text(size=9, hjust=0.5, color="grey40"),
      legend.position="right",
      plot.margin=ggplot2::margin(10,10,10,30)
    )

  p
}
