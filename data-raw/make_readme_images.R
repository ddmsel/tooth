# Run this script to generate README images
# Usage: source("data-raw/make_readme_images.R")

library(tooth)
library(ggplot2)
data(sim_exam)

# --- Image 1: Coronal odontograph ---
decay_coronal <- sim_exam |>
  dplyr::filter(tooth_surface %in% c("buc","lin","mes","dis","occ")) |>
  dplyr::group_by(tooth_num, tooth_surface) |>
  dplyr::summarise(prop = mean(lesion_code %in% 3:6 & act == 2, na.rm = TRUE),
                   .groups = "drop")

p1 <- build_odontograph(
  data = decay_coronal,
  value_col = "prop",
  teeth_per_quadrant = 7,
  title = "Active Caries by Surface",
  subtitle = "Simulated data | n = 20",
  legend_title = "Caries\nProportion",
  surfaces = c("buc","lin","mes","dis","occ"),
  show_labels = TRUE
)
ggsave("man/figures/odontograph_coronal.png", p1, width = 14, height = 5, dpi = 200)

# --- Image 2: With root surfaces ---
decay_all <- sim_exam |>
  dplyr::group_by(tooth_num, tooth_surface) |>
  dplyr::summarise(prop = mean(lesion_code %in% 3:6 & act == 2, na.rm = TRUE),
                   .groups = "drop")

p2 <- build_odontograph(
  data = decay_all,
  value_col = "prop",
  teeth_per_quadrant = 7,
  title = "Active Caries \u2014 Coronal and Root Surfaces",
  subtitle = "Simulated data | n = 20",
  legend_title = "Caries\nProportion",
  surfaces = c("buc","lin","mes","dis","occ","rootb","rootl")
)
ggsave("man/figures/odontograph_root.png", p2, width = 14, height = 5, dpi = 200)

# --- Image 3: Stratified ---
sim_exam$treatment <- ifelse(
  as.numeric(gsub("P", "", sim_exam$record_id)) <= 10, "SDF", "ART"
)

decay_strat <- sim_exam |>
  dplyr::filter(tooth_surface %in% c("buc","lin","mes","dis","occ")) |>
  dplyr::group_by(treatment, tooth_num, tooth_surface) |>
  dplyr::summarise(prop = mean(lesion_code %in% 3:6 & act == 2, na.rm = TRUE),
                   .groups = "drop")

p3 <- build_odontograph(
  data = decay_strat,
  value_col = "prop",
  teeth_per_quadrant = 7,
  title = "Active Caries",
  legend_title = "Caries\nProportion",
  surfaces = c("buc","lin","mes","dis","occ"),
  strata = "treatment",
  ncol = 1
)
ggsave("man/figures/odontograph_stratified.png", p3, width = 14, height = 10, dpi = 200)

# --- Image 4: No labels (clean) ---
p4 <- build_odontograph(
  data = decay_coronal,
  value_col = "prop",
  teeth_per_quadrant = 7,
  title = "Active Caries \u2014 No Surface Labels",
  subtitle = "show_labels = FALSE",
  legend_title = "Caries\nProportion",
  surfaces = c("buc","lin","mes","dis","occ"),
  show_labels = FALSE
)
ggsave("man/figures/odontograph_nolabels.png", p4, width = 14, height = 5, dpi = 200)

cat("README images saved to man/figures/\n")
