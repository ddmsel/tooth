#' Simulated dental examination data
#'
#' A simulated surface-level dental examination dataset for 20 patients,
#' each with 7 teeth per quadrant (28 teeth) and 5 coronal surfaces plus
#' 2 root surfaces per tooth. Designed for demonstrating and testing
#' [calc_dmft()], [calc_dmfs()], and [build_odontogram()].
#'
#' @format A data frame with 3,920 rows and 7 columns:
#' \describe{
#'   \item{record_id}{Patient identifier (character, "P01" to "P20").}
#'   \item{tooth_num}{Tooth identifier in quadrant notation (e.g. "ur1", "ll7").}
#'   \item{tooth_surface}{Surface name: buc, lin, mes, dis, occ, rootb, or rootl.}
#'   \item{lesion_code}{ICDAS lesion code (integer 0-6). 0 = sound, 3-6 = caries.}
#'   \item{filling_code}{Filling/restoration code (integer). 0 = none, 1-8 = restored.}
#'   \item{act}{Lesion activity code (integer). 1 = inactive, 2 = active.}
#'   \item{code}{Tooth-level status code (integer). 1 = missing, 2-8 = present.}
#' }
#' @examples
#' data(sim_exam)
#' head(sim_exam)
#' calc_dmft(sim_exam)
"sim_exam"
