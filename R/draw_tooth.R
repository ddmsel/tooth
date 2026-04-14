#' Draw a single tooth as polygon geometry
#'
#' Produces crown surface wedges (B/L/M/D/O) and up to four root surface
#' bars (RB/RL/RM/RD), plus labels, outlines, and diagonals — all as
#' data frames ready for ggplot2. Any surface can be excluded via the
#' `surfaces` argument.
#'
#' @param tooth_id Character label, e.g. `"ur1"`.
#' @param quadrant One of `"ur"`, `"ul"`, `"lr"`, `"ll"`.
#' @param tooth_num Numeric position within the quadrant.
#' @param is_upper Logical; TRUE for upper arch.
#' @param surface_values Data frame with `tooth_surface` and a value column.
#'   Recognised surfaces: buc, lin, mes, dis, occ, rootb, rootl, rootm, rootd.
#' @param value_col Name of the value column in `surface_values`.
#' @param x_offset,y_offset Numeric offsets for positioning.
#' @param tooth_size Numeric side length of the crown square (default 1).
#' @param show_roots Logical; draw root surface bars (default TRUE).
#' @param surfaces Character vector of surfaces to include. Omit any you
#'   don't want drawn.
#' @param display_label Optional custom label for the tooth number (e.g.
#'   FDI notation `"11"` instead of `"ur1"`). When NULL, uses `tooth_id`.
#'
#' @return Named list of tibbles: crown, roots, labels, num_label, diags, outline.
#' @export
draw_tooth <- function(tooth_id, quadrant, tooth_num, is_upper,
                       surface_values, value_col = "prop",
                       x_offset = 0, y_offset = 0,
                       tooth_size = 1, show_roots = TRUE,
                       surfaces = c("buc","lin","mes","dis","occ",
                                    "rootb","rootl","rootm","rootd"),
                       display_label = NULL) {

  sz  <- tooth_size
  ci  <- sz * 0.25

  top_key   <- ifelse(is_upper, "buc", "lin")
  bot_key   <- ifelse(is_upper, "lin", "buc")
  is_right  <- quadrant %in% c("ur", "lr")
  left_key  <- ifelse(is_right, "dis", "mes")
  right_key <- ifelse(is_right, "mes", "dis")

  short_labels <- c(buc = "B", lin = "L", mes = "M", dis = "D", occ = "O",
                    rootb = "RB", rootl = "RL", rootm = "RM", rootd = "RD")

  get_val <- function(surf) {
    v <- surface_values[surface_values$tooth_surface == surf, ]
    if (nrow(v) == 0) return(NA_real_)
    v[[value_col]][1]
  }

  x0 <- x_offset;  y0 <- y_offset
  x1 <- x0 + sz;   y1 <- y0 + sz
  cx0 <- x0 + ci;  cy0 <- y0 + ci
  cx1 <- x1 - ci;  cy1 <- y1 - ci

  # ---- Crown polygons ----
  crown_defs <- list(
    list(surface = top_key,
         x = c(x0, x1, cx1, cx0), y = c(y1, y1, cy1, cy1)),
    list(surface = bot_key,
         x = c(x0, cx0, cx1, x1), y = c(y0, cy0, cy0, y0)),
    list(surface = left_key,
         x = c(x0, cx0, cx0, x0), y = c(y1, cy1, cy0, y0)),
    list(surface = right_key,
         x = c(x1, x1, cx1, cx1), y = c(y1, y0, cy0, cy1)),
    list(surface = "occ",
         x = c(cx0, cx1, cx1, cx0), y = c(cy1, cy1, cy0, cy0))
  )
  crown_defs <- Filter(function(p) p$surface %in% surfaces, crown_defs)

  empty_poly <- tibble::tibble(x=numeric(), y=numeric(), surface=character(),
                                value=numeric(), tooth=character(), group_id=character())

  poly_df <- if (length(crown_defs) > 0) {
    purrr::map_dfr(crown_defs, function(p) {
      tibble::tibble(x = p$x, y = p$y, surface = p$surface,
                     value = get_val(p$surface), tooth = tooth_id,
                     group_id = paste0(tooth_id, "_", p$surface))
    })
  } else {
    empty_poly
  }

  # ---- Root bars: 4 surfaces ----
  root_h  <- sz * 0.15;  root_w  <- sz * 0.55
  root_side_w <- sz * 0.15;  root_side_h <- sz * 0.55
  root_cx <- x_offset + sz / 2
  root_cy <- y_offset + sz / 2

  if (is_upper) {
    rootb_y0 <- y1 + sz*0.08; rootb_y1 <- rootb_y0 + root_h
    rootl_y1 <- y0 - sz*0.08; rootl_y0 <- rootl_y1 - root_h
  } else {
    rootl_y0 <- y1 + sz*0.08; rootl_y1 <- rootl_y0 + root_h
    rootb_y1 <- y0 - sz*0.08; rootb_y0 <- rootb_y1 - root_h
  }

  mes_side <- ifelse(is_right, x1 + sz*0.08, x0 - sz*0.08 - root_side_w)
  dis_side <- ifelse(is_right, x0 - sz*0.08 - root_side_w, x1 + sz*0.08)

  make_rect <- function(x_left, y_bot, w, h) {
    list(x = c(x_left, x_left+w, x_left+w, x_left),
         y = c(y_bot+h, y_bot+h, y_bot, y_bot))
  }

  root_defs <- list(
    list(surface = "rootb",
         x = c(root_cx-root_w/2, root_cx+root_w/2, root_cx+root_w/2, root_cx-root_w/2),
         y = c(rootb_y1, rootb_y1, rootb_y0, rootb_y0)),
    list(surface = "rootl",
         x = c(root_cx-root_w/2, root_cx+root_w/2, root_cx+root_w/2, root_cx-root_w/2),
         y = c(rootl_y1, rootl_y1, rootl_y0, rootl_y0)),
    c(list(surface = "rootm"), make_rect(mes_side, root_cy-root_side_h/2, root_side_w, root_side_h)),
    c(list(surface = "rootd"), make_rect(dis_side, root_cy-root_side_h/2, root_side_w, root_side_h))
  )

  root_surfs_req <- intersect(c("rootb","rootl","rootm","rootd"), surfaces)
  roots_df <- if (show_roots && length(root_surfs_req) > 0) {
    defs <- Filter(function(p) p$surface %in% root_surfs_req, root_defs)
    purrr::map_dfr(defs, function(p) {
      tibble::tibble(x = p$x, y = p$y, surface = p$surface,
                     value = get_val(p$surface), tooth = tooth_id,
                     group_id = paste0(tooth_id, "_", p$surface))
    })
  } else {
    empty_poly
  }

  # ---- Labels ----
  label_info <- list(
    list(surf=top_key,  x=(x0+x1)/2, y=(y1+cy1)/2),
    list(surf=bot_key,  x=(x0+x1)/2, y=(y0+cy0)/2),
    list(surf=left_key, x=(x0+cx0)/2, y=(y0+y1)/2),
    list(surf=right_key,x=(x1+cx1)/2, y=(y0+y1)/2),
    list(surf="occ",    x=(cx0+cx1)/2, y=(cy0+cy1)/2)
  )
  if (show_roots) {
    label_info <- c(label_info, list(
      list(surf="rootb", x=root_cx, y=(rootb_y0+rootb_y1)/2),
      list(surf="rootl", x=root_cx, y=(rootl_y0+rootl_y1)/2),
      list(surf="rootm", x=mes_side+root_side_w/2, y=root_cy),
      list(surf="rootd", x=dis_side+root_side_w/2, y=root_cy)
    ))
  }
  label_info <- Filter(function(l) l$surf %in% surfaces, label_info)

  labels_df <- if (length(label_info) > 0) {
    tibble::tibble(
      x       = vapply(label_info, `[[`, numeric(1), "x"),
      y       = vapply(label_info, `[[`, numeric(1), "y"),
      surface = vapply(label_info, `[[`, character(1), "surf"),
      label   = unname(short_labels[vapply(label_info, `[[`, character(1), "surf")]),
      value   = vapply(label_info, function(l) get_val(l$surf), numeric(1)),
      tooth   = tooth_id
    )
  } else {
    tibble::tibble(x=numeric(), y=numeric(), surface=character(),
                   label=character(), value=numeric(), tooth=character())
  }

  # Tooth number label
  if (is_upper) {
    num_y <- (if (show_roots && "rootl" %in% surfaces) rootl_y0 else y0) - sz*0.25
  } else {
    num_y <- (if (show_roots && "rootl" %in% surfaces) rootl_y1 else y1) + sz*0.25
  }

  disp <- if (!is.null(display_label)) display_label else tooth_id
  num_label <- tibble::tibble(x=(x0+x1)/2, y=num_y, label=disp, tooth=tooth_id)

  crown_surfaces <- c("buc", "lin", "mes", "dis", "occ")
  has_crown <- any(crown_surfaces %in% surfaces)

  diag_df <- if (has_crown) {
    tibble::tibble(
      x=c(x0,x1,x0,x1), xend=c(cx0,cx1,cx0,cx1),
      y=c(y0,y0,y1,y1), yend=c(cy0,cy0,cy1,cy1), tooth=tooth_id)
  } else {
    tibble::tibble(x=numeric(), xend=numeric(), y=numeric(), yend=numeric(), tooth=character())
  }

  outline_df <- if (has_crown) {
    tibble::tibble(x=c(x0,x1,x1,x0,x0), y=c(y0,y0,y1,y1,y0), tooth=tooth_id)
  } else {
    tibble::tibble(x=numeric(), y=numeric(), tooth=character())
  }

  list(crown=poly_df, roots=roots_df, labels=labels_df,
       num_label=num_label, diags=diag_df, outline=outline_df)
}
