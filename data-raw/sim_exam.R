# Generate simulated dental examination data for the tooth package
# Run this script once to create data/sim_exam.rda
# Usage: source("data-raw/sim_exam.R")

set.seed(42)

teeth_ids <- paste0(
  rep(c("ur", "ul", "lr", "ll"), each = 7),
  rep(1:7, 4)
)

surfaces <- c("buc", "lin", "mes", "dis", "occ", "rootb", "rootl")
patients <- paste0("P", sprintf("%02d", 1:20))

sim_exam <- expand.grid(
  record_id    = patients,
  tooth_num    = teeth_ids,
  tooth_surface = surfaces,
  stringsAsFactors = FALSE
)

n <- nrow(sim_exam)

# Tooth-level status: ~5% missing (code=1), rest present (code=2-8)
tooth_status <- data.frame(
  record_id = rep(patients, each = 28),
  tooth_num = rep(teeth_ids, 20),
  stringsAsFactors = FALSE
)
tooth_status$code <- sample(
  c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L),
  nrow(tooth_status),
  replace = TRUE,
  prob = c(0.05, 0.50, 0.10, 0.05, 0.10, 0.05, 0.10, 0.05)
)

sim_exam <- merge(sim_exam, tooth_status, by = c("record_id", "tooth_num"))

# Lesion codes: mostly sound (0), some caries (3-6)
sim_exam$lesion_code <- sample(
  0:6, n, replace = TRUE,
  prob = c(0.60, 0.05, 0.05, 0.10, 0.08, 0.07, 0.05)
)

# Activity: 1=inactive, 2=active. Only relevant when lesion > 0
sim_exam$act <- ifelse(
  sim_exam$lesion_code > 0,
  sample(1:2, n, replace = TRUE, prob = c(0.4, 0.6)),
  0L
)

# Filling codes: 0=none, 1-8=restored. ~15% have fillings
sim_exam$filling_code <- sample(
  0:8, n, replace = TRUE,
  prob = c(0.85, 0.04, 0.03, 0.01, 0.02, 0.01, 0.02, 0.01, 0.01)
)

# Missing teeth have no lesions or fillings
missing_rows <- sim_exam$code == 1L
sim_exam$lesion_code[missing_rows]  <- 0L
sim_exam$act[missing_rows]          <- 0L
sim_exam$filling_code[missing_rows] <- 0L

# Sort
sim_exam <- sim_exam[order(sim_exam$record_id, sim_exam$tooth_num, sim_exam$tooth_surface), ]
rownames(sim_exam) <- NULL

usethis::use_data(sim_exam, overwrite = TRUE)
