# ============================================================================
# Compare IDs between Backbone analysis input and Rasmus regression CSV
# ============================================================================

library(readxl)
library(readr)
library(dplyr)
library(writexl)

# -------------------------------------------------------------------------
# Paths
# -------------------------------------------------------------------------

path_questionnaire <- "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389/03_analysis_input/adults_adolescents_complete_items_lt030.xlsx"
path_behavioral <- "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389/private_information/rasmus_combined_regressions_21_50sp.csv"

out_dir <- "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389/tests"
out_file <- file.path(out_dir, "compare_ids_behav_backbone_output.xlsx")

# Falls die Excel-Datei mehrere Sheets hat:
xlsx_sheet <- "combined"

# -------------------------------------------------------------------------
# Helper: detect CSV delimiter
# -------------------------------------------------------------------------

detect_delim <- function(file) {
  first_line <- readLines(file, n = 1, warn = FALSE)
  
  candidates <- c("," = ",", ";" = ";", "\t" = "\t")
  counts <- sapply(candidates, function(d) lengths(regmatches(first_line, gregexpr(d, first_line, fixed = TRUE))))
  
  names(which.max(counts))
}

# -------------------------------------------------------------------------
# Read questionnaire data: Excel
# -------------------------------------------------------------------------

questionnaire_names <- names(read_excel(path_questionnaire, sheet = xlsx_sheet, n_max = 0))

questionnaire <- read_excel(
  path_questionnaire,
  sheet = xlsx_sheet,
  col_types = rep("text", length(questionnaire_names)),
  .name_repair = "minimal"
)

if (!"vp_id" %in% names(questionnaire)) {
  stop("Column 'vp_id' was not found in questionnaire data.")
}

ids1_raw <- questionnaire$vp_id

# -------------------------------------------------------------------------
# Read behavioral data: CSV
# -------------------------------------------------------------------------

delim2 <- detect_delim(path_behavioral)

behavioral <- read_delim(
  path_behavioral,
  delim = delim2,
  col_types = cols(.default = col_character()),
  trim_ws = FALSE,
  show_col_types = FALSE,
  progress = FALSE
)

if (!"ID" %in% names(behavioral)) {
  stop("Column 'ID' was not found in behavioral data.")
}

ids2_raw <- behavioral$ID

# -------------------------------------------------------------------------
# Prepare ID vectors
# -------------------------------------------------------------------------
# Wichtig:
# - Keine Trimmung von Leerzeichen
# - Kein tolower()
# - Keine numerische Umwandlung
# - Vergleich also wirklich symbolgenau nach Text-Coercion

ids1 <- as.character(ids1_raw)
ids2 <- as.character(ids2_raw)

ids1_nonmissing <- ids1[!is.na(ids1)]
ids2_nonmissing <- ids2[!is.na(ids2)]

ids1_unique <- unique(ids1_nonmissing)
ids2_unique <- unique(ids2_nonmissing)

# -------------------------------------------------------------------------
# Compare
# -------------------------------------------------------------------------

only_in_questionnaire <- setdiff(ids1_unique, ids2_unique)
only_in_behavioral <- setdiff(ids2_unique, ids1_unique)
in_both       <- intersect(ids1_unique, ids2_unique)

summary_tbl <- tibble::tibble(
  category = c(
    "rows_questionnaire",
    "rows_behavioral",
    "nonmissing_ids_questionnaire",
    "nonmissing_ids_behavioral",
    "unique_ids_questionnaire",
    "unique_ids_behavioral",
    "duplicate_id_rows_questionnaire",
    "duplicate_id_rows_behavioral",
    "only_in_questionnaire",
    "only_in_behavioral",
    "in_both"
  ),
  n = c(
    nrow(questionnaire),
    nrow(behavioral),
    length(ids1_nonmissing),
    length(ids2_nonmissing),
    length(ids1_unique),
    length(ids2_unique),
    length(ids1_nonmissing) - length(ids1_unique),
    length(ids2_nonmissing) - length(ids2_unique),
    length(only_in_questionnaire),
    length(only_in_behavioral),
    length(in_both)
  )
)

only_in_questionnaire_tbl <- tibble::tibble(vp_id = sort(only_in_questionnaire))
only_in_behavioral_tbl <- tibble::tibble(ID   = sort(only_in_behavioral))
in_both_tbl        <- tibble::tibble(id   = sort(in_both))

# -------------------------------------------------------------------------
# Print summary
# -------------------------------------------------------------------------

cat("\n============================================================\n")
cat("ID comparison summary\n")
cat("============================================================\n\n")

cat("questionnaire data:", path_questionnaire, "\n")
cat("Sheet  :", xlsx_sheet, "\n")
cat("Column : vp_id\n\n")

cat("behavioral data:", path_behavioral, "\n")
cat("Delimiter detected:", ifelse(delim2 == "\t", "TAB", delim2), "\n")
cat("Column : ID\n\n")

print(summary_tbl)

cat("\nInterpretation based on unique, non-missing IDs:\n")
cat("- IDs in questionnaire data but not in behavioral data:", length(only_in_questionnaire), "of", length(ids1_unique), "\n")
cat("- IDs in behavioral data but not in questionnaire data:", length(only_in_behavioral), "of", length(ids2_unique), "\n")
cat("- IDs present in both tables:", length(in_both), "\n\n")

# -------------------------------------------------------------------------
# Write output
# -------------------------------------------------------------------------

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

write_xlsx(
  list(
    summary = summary_tbl,
    only_in_questionnaire = only_in_questionnaire_tbl,
    only_in_behavioral = only_in_behavioral_tbl,
    in_both = in_both_tbl
  ),
  path = out_file
)

cat("Wrote output to:\n")
cat(out_file, "\n\n")