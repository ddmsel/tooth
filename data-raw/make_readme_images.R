# Run this script to generate README images
# Usage: source("data-raw/make_readme_images.R")

library(tooth)
library(ggplot2)
data(sim_exam)

outdir <- "man/figures"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# --- Image 1: Single tooth ---
sv <- data.frame(
  tooth_surface = c("buc", "lin", "mes", "dis", "occ", "rootb", "rootl"),
  prop = c(0.12, 0.05, 0.30, 0.25, 0.45, 0.08, 0.03)
)
parts <- draw_tooth("ur6", "ur", 6, TRUE, sv,
                    surfaces = c("buc","lin","mes","dis","occ","rootb","rootl"),
                    display_label = "14")

p0 <- ggplot() +
  geom_polygon(data = parts$crown,
               aes(x = x, y = y, group = group_id, fill = value),
               color = "grey50", linewidth = 0.5) +
  geom_polygon(data = parts$roots,
               aes(x = x, y = y, group = group_id, fill = value),
               color = "grey50", linewidth = 0.5) +
  geom_segment(data = parts$diags,
               aes(x = x, y = y, xend = xend, yend = yend),
               color = "grey50", linewidth = 0.5) +
  geom_path(data = parts$outline, aes(x = x, y = y), color = "grey40", linewidth = 0.7) +
  geom_text(data = parts$labels, aes(x = x, y = y, label = label),
            size = 5, fontface = "bold", color = "grey40") +
  geom_text(data = parts$num_label, aes(x = x, y = y, label = label),
            size = 7, fontface = "bold") +
  scale_fill_gradient(low = "white", high = "#C62828", limits = c(0, 0.5),
                      name = "Caries\nProportion") +
  coord_fixed() +
  labs(title = "Single Tooth Diagram",
       subtitle = "5 coronal + 2 root surfaces | FDI tooth 14") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
        plot.background = element_rect(fill = "white", color = NA))
ggsave(file.path(outdir, "single_tooth.png"), p0, width = 5, height = 5, dpi = 200)

# --- Image 2: Coronal odontogram ---
decay_coronal <- sim_exam |>
  dplyr::filter(tooth_surface %in% c("buc","lin","mes","dis","occ")) |>
  dplyr::group_by(tooth_num, tooth_surface) |>
  dplyr::summarise(prop = mean(lesion_code %in% 3:6 & act == 2, na.rm = TRUE),
                   .groups = "drop")

p1 <- build_odontogram(
  data = decay_coronal,
  teeth_per_quadrant = 7,
  title = "Active Caries by Surface",
  subtitle = "Simulated data | n = 20",
  legend_title = "Caries\nProportion",
  surfaces = c("buc","lin","mes","dis","occ"),
  tooth_label_size = 3
)
ggsave(file.path(outdir, "odontogram_coronal.png"), p1, width = 14, height = 5, dpi = 200)

# --- Image 3: With root surfaces ---
decay_all <- sim_exam |>
  dplyr::group_by(tooth_num, tooth_surface) |>
  dplyr::summarise(prop = mean(lesion_code %in% 3:6 & act == 2, na.rm = TRUE),
                   .groups = "drop")

p2 <- build_odontogram(
  data = decay_all,
  teeth_per_quadrant = 7,
  title = "Active Caries \u2014 Coronal and Root Surfaces",
  subtitle = "Simulated data | n = 20",
  legend_title = "Caries\nProportion",
  surfaces = c("buc","lin","mes","dis","occ","rootb","rootl"),
  tooth_label_size = 3
)
ggsave(file.path(outdir, "odontogram_root.png"), p2, width = 14, height = 5, dpi = 200)

# --- Image 4: Stratified with stats and footnote ---
sim_exam$treatment <- ifelse(
  as.numeric(gsub("P", "", sim_exam$record_id)) <= 10, "SDF", "ART"
)

decay_strat <- sim_exam |>
  dplyr::filter(tooth_surface %in% c("buc","lin","mes","dis","occ")) |>
  dplyr::group_by(treatment, tooth_num, tooth_surface) |>
  dplyr::summarise(prop = mean(lesion_code %in% 3:6 & act == 2, na.rm = TRUE),
                   .groups = "drop")

stats_df <- data.frame(
  treatment = c("SDF", "ART"),
  n = c(10, 10),
  mean_DT = c(3.2, 4.1),
  mean_DMFT = c(6.8, 7.5)
)

p3 <- build_odontogram(
  data = decay_strat,
  teeth_per_quadrant = 7,
  title = "Active Caries",
  legend_title = "Caries\nProportion",
  surfaces = c("buc","lin","mes","dis","occ"),
  strata = "treatment",
  stats = stats_df,
  footnote = "B=Buccal, L=Lingual, M=Mesial, D=Distal, O=Occlusal. * p<0.05, ** p<0.01, *** p<0.001.",
  ncol = 1
)
ggsave(file.path(outdir, "odontogram_stratified.png"), p3, width = 14, height = 10, dpi = 200)

# --- Image 5: No labels ---
p4 <- build_odontogram(
  data = decay_coronal,
  teeth_per_quadrant = 7,
  title = "Active Caries \u2014 No Surface Labels",
  subtitle = "show_labels = FALSE",
  legend_title = "Caries\nProportion",
  surfaces = c("buc","lin","mes","dis","occ"),
  show_labels = FALSE,
  tooth_label_size = 3
)
ggsave(file.path(outdir, "odontogram_nolabels.png"), p4, width = 14, height = 5, dpi = 200)

# --- Image 6: FDI numbering ---
p5 <- build_odontogram(
  data = decay_coronal,
  teeth_per_quadrant = 7,
  title = "Active Caries \u2014 FDI Numbering",
  legend_title = "Caries\nProportion",
  surfaces = c("buc","lin","mes","dis","occ"),
  numbering = "fdi",
  tooth_label_size = 3
)
ggsave(file.path(outdir, "odontogram_fdi.png"), p5, width = 14, height = 5, dpi = 200)

cat("README images saved to", outdir, "\n")
