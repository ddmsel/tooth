#' Build a surface-level odontogram heatmap
#'
#' Draws a full-arch odontogram with colour-coded tooth surfaces.
#' Supports primary and permanent dentition (5–8 teeth per quadrant),
#' selective surfaces, stratification, summary statistics with significance
#' testing, and configurable tooth numbering.
#'
#' @param data Data frame with at least `tooth_num` (e.g. `"ur1"`),
#'   `tooth_surface` (e.g. `"buc"`, `"occ"`, `"rootb"`), and a numeric value
#'   column. If stratifying, include the column named in `strata`.
#' @param value_col Name of the numeric column to map to fill colour.
#' @param dentition `"permanent"` or `"primary"`.
#' @param teeth_per_quadrant Integer 5–8 (default derived from `dentition`).
#' @param title,subtitle Plot title/subtitle.
#' @param color_low,color_high Gradient endpoints (default `"#FFFFFF"` to
#'   `"#C62828"`).
#' @param na_color Fill for missing surfaces (default `"grey90"`).
#' @param min_val,max_val Lower and upper limits for the colour scale.
#'   Both auto-detected when NULL. Set manually to fix the range across
#'   multiple plots.
#' @param legend_title Legend title.
#' @param show_roots Logical; draw root caries bars.
#' @param surfaces Character vector of surfaces to draw.
#' @param show_labels Logical; show surface abbreviation labels (B, L, M, D,
#'   O, RB, RL, RM, RD) inside each surface polygon.
#' @param label_size Numeric; size of surface labels.
#' @param tooth_label_size Numeric; size of tooth number labels (default 3).
#' @param numbering Tooth numbering system for display: `"quadrant"` (default),
#'   `"fdi"`, or `"universal"`.
#' @param strata Optional column name for stratification.
#' @param strata_labels Optional named character vector to relabel strata in
#'   panel titles.
#' @param stats Optional data frame of per-group summary statistics to display
#'   below each panel. Must contain a column matching `strata` and numeric
#'   columns to display (e.g. `n`, `mean_DMFT`, `mean_DT`). See Details.
#' @param stats_test Method for comparing stats across strata: `"t"` for
#'   t-test (default), `"wilcox"` for Wilcoxon rank-sum, or `NULL` to skip.
#'   P-values are shown as significance stars.
#' @param stats_var Column name in `stats_raw` to test for differences (e.g.
#'   `"DMFT"`). Required when `stats_test` is not NULL.
#' @param stats_raw Optional data frame of individual-level data used for
#'   significance testing. Must contain a column matching `strata` and a
#'   numeric column matching `stats_var`. Required when `stats_test` is set.
#' @param footnote Optional character string displayed below the plot as a
#'   caption. Use for abbreviation explanations and p-value definitions.
#' @param combine Logical; if `strata` is set, combine panels into one plot
#'   (default TRUE) or return a named list.
#' @param ncol Number of columns when combining stratified panels.
#'
#' @details
#' **Summary statistics:** Pass a data frame to `stats` with one row per
#' stratum. Example:
#' ```
#' stats_df <- data.frame(
#'   treatment = c("SDF", "ART"),
#'   n = c(120, 115),
#'   mean_DT = c(2.3, 2.8),
#'   mean_DMFT = c(5.1, 5.6)
#' )
#' build_odontogram(d, strata = "treatment", stats = stats_df)
#' ```
#'
#' **Significance stars:** When `stats_test` and `stats_var` are set and
#' raw data is provided via `stats_raw`, p-values are computed and appended:
#' `***` p < 0.001, `**` p < 0.01, `*` p < 0.05, ns otherwise.
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
#' build_odontogram(d, teeth_per_quadrant = 7)
build_odontogram <- function(data, value_col = "prop",
                             dentition = "permanent",
                             teeth_per_quadrant = NULL,
                             title = "Surface Odontogram",
                             subtitle = NULL,
                             color_low = "#FFFFFF",
                             color_high = "#C62828",
                             na_color = "grey90",
                             min_val = NULL, max_val = NULL,
                             legend_title = "Proportion",
                             show_roots = TRUE,
                             surfaces = c("buc","lin","mes","dis","occ",
                                          "rootb","rootl","rootm","rootd"),
                             show_labels = TRUE,
                             label_size = 1.8,
                             tooth_label_size = 3,
                             numbering = c("quadrant", "fdi", "universal"),
                             strata = NULL,
                             strata_labels = NULL,
                             stats = NULL,
                             stats_test = NULL,
                             stats_var = NULL,
                             stats_raw = NULL,
                             footnote = NULL,
                             combine = TRUE,
                             ncol = 1) {

  numbering <- match.arg(numbering)

  # --- Compute p-value string for strata comparison ---
  pval_string <- ""
  if (!is.null(stats_test) && !is.null(stats_var) &&
      !is.null(stats_raw) && !is.null(strata)) {
    groups <- split(stats_raw[[stats_var]], stats_raw[[strata]])
    if (length(groups) == 2) {
      g1 <- groups[[1]]; g2 <- groups[[2]]
      pval <- tryCatch({
        if (stats_test == "wilcox") {
          stats::wilcox.test(g1, g2)$p.value
        } else {
          stats::t.test(g1, g2)$p.value
        }
      }, error = function(e) NA_real_)

      if (!is.na(pval)) {
        stars <- if (pval < 0.001) "***" else if (pval < 0.01) "**" else
                 if (pval < 0.05) "*" else "ns"
        test_name <- if (stats_test == "wilcox") "Wilcoxon" else "t-test"
        pval_string <- paste0("  (", test_name, " p = ",
                              formatC(pval, format = "f", digits = 3),
                              " ", stars, ")")
      }
    }
  }

  # --- Handle stratification ---
  if (!is.null(strata)) {
    strata_vals <- unique(data[[strata]])
    plots <- lapply(strata_vals, function(sv) {
      sub_data <- data[data[[strata]] == sv, ]
      display_label <- if (!is.null(strata_labels) &&
                           as.character(sv) %in% names(strata_labels)) {
        strata_labels[[as.character(sv)]]
      } else {
        as.character(sv)
      }

      # Build stats subtitle
      stats_sub <- NULL
      if (!is.null(stats) && strata %in% names(stats)) {
        row <- stats[stats[[strata]] == sv, , drop = FALSE]
        if (nrow(row) > 0) {
          stat_cols <- setdiff(names(row), strata)
          parts <- vapply(stat_cols, function(col) {
            val <- row[[col]][1]
            if (is.numeric(val)) {
              paste0(col, " = ", formatC(val, format = "f", digits = 1))
            } else {
              paste0(col, " = ", val)
            }
          }, character(1))
          stats_sub <- paste(parts, collapse = " | ")
        }
      }

      panel_subtitle <- if (!is.null(stats_sub)) {
        paste0(stats_sub, if (nzchar(pval_string)) pval_string else "")
      } else {
        subtitle
      }

      build_odontogram(
        data = sub_data, value_col = value_col,
        dentition = dentition, teeth_per_quadrant = teeth_per_quadrant,
        title = paste0(title, " \u2014 ", display_label),
        subtitle = panel_subtitle,
        color_low = color_low, color_high = color_high,
        na_color = na_color, min_val = min_val, max_val = max_val,
        legend_title = legend_title, show_roots = show_roots,
        surfaces = surfaces, show_labels = show_labels,
        label_size = label_size, tooth_label_size = tooth_label_size,
        numbering = numbering, strata = NULL, footnote = NULL
      )
    })
    names(plots) <- as.character(strata_vals)

    if (combine) {
      combined <- patchwork::wrap_plots(plots, ncol = ncol)
      if (!is.null(footnote)) {
        combined <- combined + patchwork::plot_annotation(
          caption = footnote,
          theme = ggplot2::theme(
            plot.caption = ggplot2::element_text(
              size = 7, hjust = 0, color = "grey40",
              margin = ggplot2::margin(t = 10)),
            plot.background = ggplot2::element_rect(fill = "white", color = NA)
          )
        )
      }
      return(combined)
    }
    return(plots)
  }

  # --- Determine root display from surfaces arg ---
  has_roots <- any(c("rootb","rootl","rootm","rootd") %in% surfaces)
  has_side_roots <- any(c("rootm","rootd") %in% surfaces)
  show_roots <- show_roots & has_roots

  cfg <- tooth_config(dentition, teeth_per_quadrant)
  tpq <- max(cfg$num)

  tooth_size   <- 1
  gap          <- if (has_side_roots && show_roots) 0.75 else 0.45
  midline_gap  <- if (has_side_roots && show_roots) 1.0 else 0.7
  row_gap      <- 1.0

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

  # Convert tooth labels if needed
  if (numbering != "quadrant") {
    positions$display_label <- tooth_convert(
      positions$tooth_id, from = "quadrant", to = numbering,
      dentition = dentition
    )
  } else {
    positions$display_label <- positions$tooth_id
  }

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
      surfaces=surfaces,
      display_label=pos$display_label
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

  # Colour scale limits
  all_vals <- c(crown_df$value, roots_df$value)
  if (is.null(min_val)) min_val <- 0
  if (is.null(max_val)) {
    max_val <- if (length(all_vals) > 0) max(all_vals, na.rm=TRUE) else 1
    if (is.infinite(max_val) || is.na(max_val)) max_val <- 1
  }

  midline_x <- max(upper_right$x_pos) + tooth_size + midline_gap/2
  upper_y   <- row_gap + tooth_size/2
  lower_y   <- -tooth_size - row_gap + tooth_size/2
  label_x   <- -1.0

  side_label_y <- if (nrow(outlines_df) > 0) {
    max(outlines_df$y) + 0.7
  } else if (nrow(roots_df) > 0) {
    max(roots_df$y) + 0.7
  } else {
    row_gap + tooth_size + 0.7
  }

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

  p <- p +
    ggplot2::geom_text(data=nums_df,
      ggplot2::aes(x=.data$x, y=.data$y, label=toupper(.data$label)),
      size=tooth_label_size, color="grey30", fontface="bold") +
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
                                 na.value=na_color, limits=c(min_val, max_val),
                                 name=legend_title) +
    ggplot2::coord_fixed() +
    ggplot2::labs(title=title, subtitle=subtitle,
                  caption=footnote) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title=ggplot2::element_text(face="bold", size=12, hjust=0.5),
      plot.subtitle=ggplot2::element_text(size=9, hjust=0.5, color="grey40"),
      plot.caption=ggplot2::element_text(size=7, hjust=0, color="grey40",
                                          margin=ggplot2::margin(t=10)),
      legend.position="right",
      plot.margin=ggplot2::margin(10,10,10,30),
      plot.background=ggplot2::element_rect(fill="white", color=NA),
      panel.background=ggplot2::element_rect(fill="white", color=NA)
    )

  p
}
