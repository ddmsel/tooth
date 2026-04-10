# tooth

Dental public health analysis tools for R.

## Features

- **Odontograph heatmaps** with 5 coronal surfaces (B/L/M/D/O) + 4 root surfaces (RB/RL/RM/RD)
- **Selective surfaces**: show coronal-only, root-only, or any subset
- **Stratified odontographs**: facet by treatment arm, site, or any variable
- **DMFT/dmft and DMFS/dmfs** with separate root vs coronal caries tallies
- **Long or wide format** input (auto-pivots wide → long)
- **Primary and permanent** dentition, 5–8 teeth per quadrant
- **Tooth numbering** conversion: FDI ↔ Universal ↔ quadrant

## Installation

```r
devtools::install("path/to/tooth")
# or
install.packages("tooth_0.2.0.tar.gz", repos = NULL, type = "source")
```

## Odontograph Examples

### Coronal surfaces only (7 teeth/quadrant)

```r
library(tooth)
d <- expand.grid(
  tooth_num     = paste0(rep(c("ur","ul","lr","ll"), each = 7), 1:7),
  tooth_surface = c("buc","lin","mes","dis","occ"),
  stringsAsFactors = FALSE
)
d$prop <- runif(nrow(d), 0, 0.4)

build_odontograph(d, teeth_per_quadrant = 7,
                  surfaces = c("buc","lin","mes","dis","occ"))
```

### Root caries only

```r
d_root <- expand.grid(
  tooth_num     = paste0(rep(c("ur","ul","lr","ll"), each = 7), 1:7),
  tooth_surface = c("rootb","rootl","rootm","rootd"),
  stringsAsFactors = FALSE
)
d_root$prop <- runif(nrow(d_root), 0, 0.2)

build_odontograph(d_root, teeth_per_quadrant = 7,
                  surfaces = c("rootb","rootl","rootm","rootd"),
                  title = "Root Caries Odontograph",
                  color_high = "#4A148C")
```

### Primary dentition

```r
build_odontograph(d_primary, dentition = "primary",
                  surfaces = c("buc","lin","mes","dis","occ"),
                  show_roots = FALSE,
                  title = "Primary Dentition Caries")
```

### Stratified by treatment arm

```r
# data must include a 'treatment' column
build_odontograph(d, strata = "treatment",
                  title = "Caries by Arm", ncol = 1)
```

## DMFT / DMFS Examples

### Basic (long format)

```r
# One row per tooth-surface with: record_id, tooth_num, tooth_surface,
# lesion_code, act, filling_code, code
results <- calc_dmft(surface_data)
results <- calc_dmfs(surface_data)
```

### With root caries separated

```r
results <- calc_dmft(surface_data,
                     root_lesion_col = "lesion_code")
# Returns DT (coronal) + RDT (root) separately
```

### Repeated measures + stratification

```r
results <- calc_dmft(surface_data,
                     group = "redcap_event_name",
                     strata = "treatment")
```

### Wide format input

```r
# Columns like: ur1_buc_les, ur1_buc_fil, ur1_buc_act, ur1_code
results <- calc_dmft(wide_data, format = "wide")
```

### Custom coding (primary dentition)

```r
results <- calc_dmft(primary_data,
                     decayed_codes = c(3,4,5,6),
                     filled_codes = c(1,2),
                     present_codes = c(2,3,4,5))
```

## Tooth Number Conversion

```r
tooth_convert("11", from = "fdi", to = "quadrant")
# "ur1"
tooth_convert("ur1", from = "quadrant", to = "universal")
# "8"
```

## All Functions

| Function | Description |
|---|---|
| `build_odontograph()` | Full-arch surface heatmap with stratification |
| `draw_tooth()` | Single tooth polygon geometry (9 surfaces) |
| `calc_dmft()` | DMFT/dmft with root/coronal separation |
| `calc_dmfs()` | DMFS/dmfs with root/coronal separation |
| `pivot_to_long()` | Wide → long format converter |
| `tooth_config()` | Arch layout configuration |
| `tooth_convert()` | FDI ↔ Universal ↔ quadrant numbering |
