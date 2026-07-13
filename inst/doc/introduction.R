## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 14,
  fig.height = 5,
  dpi = 100
)

## -----------------------------------------------------------------------------
library(tooth)
data(sim_exam)
head(sim_exam)

## -----------------------------------------------------------------------------
dmft <- calc_dmft(sim_exam)
head(dmft)

## -----------------------------------------------------------------------------
summary(dmft[, c("DT", "FT", "MT", "DMFT", "num_teeth")])

## -----------------------------------------------------------------------------
sim_exam$treatment <- ifelse(
  as.numeric(gsub("P", "", sim_exam$record_id)) <= 10, "SDF", "ART"
)

dmft_by_arm <- calc_dmft(sim_exam, strata = "treatment")
head(dmft_by_arm)

## -----------------------------------------------------------------------------
dmft_root <- calc_dmft(sim_exam, root_lesion_col = "lesion_code")
head(dmft_root[, c("record_id", "DT", "MT", "DMFT", "RDT")])

## -----------------------------------------------------------------------------
dmfs <- calc_dmfs(sim_exam)
head(dmfs)

## ----fig.width=5, fig.height=5------------------------------------------------
library(ggplot2)

sv <- data.frame(
  tooth_surface = c("buc", "lin", "mes", "dis", "occ", "rootb", "rootl"),
  prop = c(0.12, 0.05, 0.30, 0.25, 0.45, 0.08, 0.03)
)

parts <- draw_tooth("ur1", "ur", 1, TRUE, sv,
                    surfaces = c("buc","lin","mes","dis","occ","rootb","rootl"),
                    display_label = "FDI 11")

ggplot() +
  geom_polygon(data = parts$crown,
               aes(x = x, y = y, group = group_id, fill = value),
               color = "grey50") +
  geom_polygon(data = parts$roots,
               aes(x = x, y = y, group = group_id, fill = value),
               color = "grey50") +
  geom_segment(data = parts$diags,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "grey50") +
  geom_path(data = parts$outline, aes(x = x, y = y),
            color = "grey40") +
  geom_text(data = parts$labels,
            aes(x = x, y = y, label = label),
            size = 5, fontface = "bold", color = "grey40") +
  geom_text(data = parts$num_label,
            aes(x = x, y = y, label = label),
            size = 7, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "#C62828",
                      limits = c(0, 0.5), name = "Proportion") +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA))

## -----------------------------------------------------------------------------
library(dplyr)

decay_prop <- sim_exam %>%
  filter(tooth_surface %in% c("buc", "lin", "mes", "dis", "occ")) %>%
  group_by(tooth_num, tooth_surface) %>%
  summarise(
    prop = mean(lesion_code %in% 3:6 & act == 2, na.rm = TRUE),
    .groups = "drop"
  )

## -----------------------------------------------------------------------------
build_odontogram(
  data = decay_prop,
  teeth_per_quadrant = 7,
  title = "Active Caries Proportion by Surface",
  legend_title = "Caries\nProportion",
  surfaces = c("buc", "lin", "mes", "dis", "occ")
)

## -----------------------------------------------------------------------------
decay_all <- sim_exam %>%
  group_by(tooth_num, tooth_surface) %>%
  summarise(
    prop = mean(lesion_code %in% 3:6 & act == 2, na.rm = TRUE),
    .groups = "drop"
  )

build_odontogram(
  data = decay_all,
  teeth_per_quadrant = 7,
  title = "Active Caries \u2014 Coronal and Root Surfaces",
  legend_title = "Caries\nProportion",
  surfaces = c("buc", "lin", "mes", "dis", "occ", "rootb", "rootl")
)

## -----------------------------------------------------------------------------
build_odontogram(
  data = decay_prop,
  teeth_per_quadrant = 7,
  title = "Active Caries \u2014 Clean Export",
  surfaces = c("buc", "lin", "mes", "dis", "occ"),
  show_labels = FALSE,
  tooth_label_size = 3.5
)

## -----------------------------------------------------------------------------
build_odontogram(
  data = decay_prop,
  teeth_per_quadrant = 7,
  title = "Active Caries \u2014 FDI Numbering",
  surfaces = c("buc", "lin", "mes", "dis", "occ"),
  numbering = "fdi"
)

## -----------------------------------------------------------------------------
build_odontogram(
  data = decay_prop,
  teeth_per_quadrant = 7,
  title = "Fixed Scale (0 to 0.5)",
  surfaces = c("buc", "lin", "mes", "dis", "occ"),
  min_val = 0, max_val = 0.5
)

## ----fig.height=10------------------------------------------------------------
decay_by_arm <- sim_exam %>%
  filter(tooth_surface %in% c("buc", "lin", "mes", "dis", "occ")) %>%
  group_by(treatment, tooth_num, tooth_surface) %>%
  summarise(
    prop = mean(lesion_code %in% 3:6 & act == 2, na.rm = TRUE),
    .groups = "drop"
  )

build_odontogram(
  data = decay_by_arm,
  teeth_per_quadrant = 7,
  title = "Active Caries",
  legend_title = "Caries\nProportion",
  surfaces = c("buc", "lin", "mes", "dis", "occ"),
  strata = "treatment",
  ncol = 1
)

## ----fig.height=10------------------------------------------------------------
build_odontogram(
  data = decay_by_arm,
  teeth_per_quadrant = 7,
  title = "Active Caries",
  surfaces = c("buc", "lin", "mes", "dis", "occ"),
  strata = "treatment",
  strata_labels = c("SDF" = "Silver Diamine Fluoride",
                    "ART" = "Atraumatic Restorative Treatment"),
  ncol = 1
)

## ----fig.height=10------------------------------------------------------------
# Compute stats from DMFT results
dmft_summary <- dmft_by_arm %>%
  group_by(treatment) %>%
  summarise(
    n = dplyr::n(),
    mean_DT = round(mean(DT, na.rm = TRUE), 1),
    mean_DMFT = round(mean(DMFT, na.rm = TRUE), 1),
    .groups = "drop"
  )
dmft_summary

build_odontogram(
  data = decay_by_arm,
  teeth_per_quadrant = 7,
  title = "Active Caries",
  surfaces = c("buc", "lin", "mes", "dis", "occ"),
  strata = "treatment",
  stats = dmft_summary,
  ncol = 1
)

## ----fig.height=10------------------------------------------------------------
build_odontogram(
  data = decay_by_arm,
  teeth_per_quadrant = 7,
  title = "Active Caries",
  surfaces = c("buc", "lin", "mes", "dis", "occ"),
  strata = "treatment",
  stats = dmft_summary,
  stats_test = "wilcox",
  stats_var = "DMFT",
  stats_raw = dmft_by_arm,
  ncol = 1
)

## ----fig.height=11------------------------------------------------------------
build_odontogram(
  data = decay_by_arm,
  teeth_per_quadrant = 7,
  title = "Active Caries",
  surfaces = c("buc", "lin", "mes", "dis", "occ"),
  strata = "treatment",
  stats = dmft_summary,
  stats_test = "wilcox",
  stats_var = "DMFT",
  stats_raw = dmft_by_arm,
  footnote = paste0(
    "B = Buccal, L = Lingual, M = Mesial, D = Distal, O = Occlusal.\n",
    "DT = Decayed Teeth, DMFT = Decayed/Missing/Filled Teeth.\n",
    "* p < 0.05, ** p < 0.01, *** p < 0.001 (Wilcoxon rank-sum test)."
  ),
  ncol = 1
)

## ----fig.width=12-------------------------------------------------------------
primary_teeth <- paste0(
  rep(c("ur", "ul", "lr", "ll"), each = 5), rep(1:5, 4)
)
d_primary <- expand.grid(
  tooth_num = primary_teeth,
  tooth_surface = c("buc", "lin", "mes", "dis", "occ"),
  stringsAsFactors = FALSE
)
d_primary$prop <- runif(nrow(d_primary), 0, 0.5)

build_odontogram(
  data = d_primary,
  dentition = "primary",
  title = "Primary Dentition \u2014 Simulated Caries",
  color_high = "#E65100",
  surfaces = c("buc", "lin", "mes", "dis", "occ"),
  show_roots = FALSE
)

## -----------------------------------------------------------------------------
tooth_convert("11", from = "fdi", to = "quadrant")
tooth_convert("ur1", from = "quadrant", to = "universal")
tooth_convert(c("11", "21", "36", "46"), from = "fdi", to = "quadrant")

## -----------------------------------------------------------------------------
tooth_config("permanent", teeth_per_quadrant = 7)
tooth_config("primary")

