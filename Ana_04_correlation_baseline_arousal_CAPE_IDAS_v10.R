#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ana_04_correlation_baseline_arousal_CAPE_IDAS.R
# Correlations of pupil/arousal measures with CAPE and IDAS scores
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ---- Settings ---------------------------------------------------------------

pupil_file <- paste0(
  "K:/Wilken_Arbeitsordner/Raw_data/RU5389_Integration/",
  "combined_pupil_arousal_data.csv"
)

clinical_file <- paste0(
  "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389/03_analysis_input/",
  "adults_adolescents_HiTOP_subscales_enriched.xlsx"
)

output_dir <- file.path(
  "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389/out/arousal_analysis",
  paste0(
    format(Sys.Date(), "%Y-%m-%d"),
    "_correlation_baseline_arousal_CAPE_IDAS"
  )
)

# Only BaselineArousal is correlated with CAPE and IDAS.
correlation_pupil_vars <- "BaselineArousal"

# The raw dilation measures are retained only for descriptive distribution
# plots and are not included in any correlation.
distribution_pupil_vars <- c(
  "BaselineArousal",
  "RawBlackDilation",
  "RawWhiteDilation",
  "RawGrayDilation"
)

# Only IDs, project/session information, and variables used in the analyses
# are checked for missing values. Optional metadata columns may remain empty.
check_all_input_columns_for_missing <- FALSE

# Every pupil participant must have exactly one a1 and one a2 row.
require_both_sessions <- TRUE

alpha <- 0.05


# ---- Packages ---------------------------------------------------------------

required_packages <- c(
  "tidyverse",
  "readxl",
  "openxlsx",
  "lme4",
  "lmerTest",
  "emmeans"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing packages: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them with install.packages().",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(openxlsx)
  library(lme4)
  library(lmerTest)
  library(emmeans)
})


# ---- Output folders ---------------------------------------------------------

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

dir_data <- file.path(output_dir, "data")
dir_tables <- file.path(output_dir, "tables")
dir_distributions <- file.path(output_dir, "plots", "distributions")
dir_scatter <- file.path(output_dir, "plots", "scatterplots")
dir_interactions <- file.path(output_dir, "plots", "project_session_interactions")

walk(
  c(
    dir_data,
    dir_tables,
    dir_distributions,
    dir_scatter,
    dir_interactions
  ),
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)


# ---- Helper functions -------------------------------------------------------

normalize_id <- function(x) {
  if (is.numeric(x)) {
    x <- format(x, scientific = FALSE, trim = TRUE, digits = 22)
  }

  x |>
    as.character() |>
    str_trim() |>
    str_replace_all("\\s+", "") |>
    str_replace("\\.0+$", "")
}

normalize_project <- function(x) {
  x <- x |>
    as.character() |>
    str_trim() |>
    str_to_upper() |>
    str_replace_all("\\s+", "")

  # Accept standard labels such as P8 and extended labels such as P8_BERLIN.
  # Only the leading project number is used for matching.
  number <- str_match(x, "^P?([0-9]+)(?:_.*)?$")[, 2]

  if (anyNA(number)) {
    stop(
      "Invalid project values: ",
      paste(unique(x[is.na(number)]), collapse = ", "),
      call. = FALSE
    )
  }

  paste0("P", as.integer(number))
}

write_xlsx_table <- function(data, path, sheet_name = "data") {
  workbook <- createWorkbook()
  addWorksheet(workbook, sheet_name)

  writeData(
    workbook,
    sheet = sheet_name,
    x = data,
    withFilter = nrow(data) > 0
  )

  freezePane(workbook, sheet = sheet_name, firstRow = TRUE)

  if (ncol(data) > 0) {
    setColWidths(
      workbook,
      sheet = sheet_name,
      cols = seq_len(ncol(data)),
      widths = "auto"
    )
  }

  saveWorkbook(workbook, path, overwrite = TRUE)
}

write_error_and_stop <- function(data, filename, message_text) {
  filename <- str_replace(filename, "\\.csv$", ".xlsx")
  path <- file.path(dir_tables, filename)
  write_xlsx_table(data, path, sheet_name = "diagnostic")
  stop(message_text, "\nDiagnostic file: ", path, call. = FALSE)
}

check_columns <- function(data, required, data_name) {
  missing_columns <- setdiff(required, names(data))

  if (length(missing_columns) > 0) {
    stop(
      "Missing columns in ", data_name, ": ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
}

check_missing <- function(data, columns, data_name) {
  missing_table <- tibble(
    variable = columns,
    n_missing_or_blank = map_int(
      columns,
      ~ {
        x <- data[[.x]]
        missing <- is.na(x)

        if (is.character(x)) {
          missing <- missing | str_trim(x) == ""
        }

        sum(missing)
      }
    )
  ) |>
    filter(n_missing_or_blank > 0)

  if (nrow(missing_table) > 0) {
    write_error_and_stop(
      missing_table,
      paste0(
        "ERROR_missing_",
        str_replace_all(data_name, "[^A-Za-z0-9]+", "_"),
        ".xlsx"
      ),
      paste0(
        "Missing or blank values found in ", data_name,
        ". The analysis stops rather than deleting cases."
      )
    )
  }
}

check_numeric <- function(data, columns, data_name) {
  non_numeric <- columns[!map_lgl(data[columns], is.numeric)]

  if (length(non_numeric) > 0) {
    stop(
      "Non-numeric analysis variables in ", data_name, ": ",
      paste(non_numeric, collapse = ", "),
      "\nCheck import settings and decimal separators.",
      call. = FALSE
    )
  }
}

check_variance <- function(data, columns, data_name) {
  constant <- columns[
    map_lgl(columns, ~ n_distinct(data[[.x]]) < 2)
  ]

  if (length(constant) > 0) {
    stop(
      "Variables with zero variance in ", data_name, ": ",
      paste(constant, collapse = ", "),
      call. = FALSE
    )
  }
}

effect_size_label <- function(r) {
  case_when(
    abs(r) < 0.10 ~ "negligible",
    abs(r) < 0.30 ~ "small",
    abs(r) < 0.50 ~ "medium",
    TRUE ~ "large"
  )
}

format_p <- function(p) {
  ifelse(
    p < 0.001,
    "< .001",
    paste0("= ", formatC(p, format = "f", digits = 3))
  )
}

run_correlation <- function(data, analysis_set, pupil_var, clinical_var) {
  test <- cor.test(
    data[[clinical_var]],
    data[[pupil_var]],
    method = "pearson",
    alternative = "two.sided",
    conf.level = 0.95
  )

  r <- unname(test$estimate)

  tibble(
    analysis_set = analysis_set,
    pupil_measure = pupil_var,
    clinical_measure = clinical_var,
    n_participants = n_distinct(data$id_std),
    r = r,
    ci_low_95 = unname(test$conf.int[1]),
    ci_high_95 = unname(test$conf.int[2]),
    r_squared = r^2,
    effect_magnitude = effect_size_label(r),
    direction = if_else(r >= 0, "positive", "negative"),
    t_value = unname(test$statistic),
    degrees_of_freedom = unname(test$parameter),
    p_value = test$p.value,
    pupil_mean = mean(data[[pupil_var]]),
    pupil_sd = sd(data[[pupil_var]]),
    clinical_mean = mean(data[[clinical_var]]),
    clinical_sd = sd(data[[clinical_var]])
  )
}


# ---- Read input files -------------------------------------------------------

if (!file.exists(pupil_file)) {
  stop("Pupil file not found: ", pupil_file, call. = FALSE)
}

if (!file.exists(clinical_file)) {
  stop("Clinical file not found: ", clinical_file, call. = FALSE)
}

pupil_raw <- read_csv(
  pupil_file,
  col_types = cols(
    SubjectID = col_character(),
    Session = col_character(),
    BaselineArousal = col_double(),
    RawBlackDilation = col_double(),
    RawWhiteDilation = col_double(),
    RawGrayDilation = col_double(),
    project = col_character()
  ),
  na = c("", "NA", "NaN", "NULL"),
  trim_ws = TRUE,
  show_col_types = FALSE
)

clinical_raw <- read_xlsx(
  clinical_file,
  sheet = "combined",
  na = c("", "NA", "NaN", "NULL"),
  trim_ws = TRUE
)

check_columns(
  pupil_raw,
  c("SubjectID", "Session", distribution_pupil_vars, "project"),
  "pupil CSV"
)

check_columns(
  clinical_raw,
  c(
    "vp_id",
    "project",
    "score_cape",
    "score_cape__distress",
    "score_cape__frequency",
    "score_idas"
  ),
  "clinical Excel file"
)

clinical_vars <- names(clinical_raw) |>
  str_subset("^score_(cape|idas)($|__)")

if (length(clinical_vars) == 0) {
  stop("No CAPE or IDAS score columns detected.", call. = FALSE)
}


# ---- Missing-value checks ---------------------------------------------------

if (check_all_input_columns_for_missing) {
  check_missing(pupil_raw, names(pupil_raw), "pupil_input_all_columns")
  check_missing(clinical_raw, names(clinical_raw), "clinical_input_all_columns")
} else {
  check_missing(
    pupil_raw,
    c("SubjectID", "Session", distribution_pupil_vars, "project"),
    "pupil_analysis_columns"
  )

  check_missing(
    clinical_raw,
    c("vp_id", "project", clinical_vars),
    "clinical_analysis_columns"
  )
}


# ---- Clean and validate IDs, projects, and sessions -------------------------

pupil <- pupil_raw |>
  mutate(
    id_std = normalize_id(SubjectID),
    project_std = normalize_project(project),
    Session = Session |> str_trim() |> str_to_lower()
  )

clinical <- clinical_raw |>
  mutate(
    id_std = normalize_id(vp_id),
    project_std = normalize_project(project)
  )

check_numeric(pupil, distribution_pupil_vars, "pupil file")
check_numeric(clinical, clinical_vars, "clinical file")

invalid_sessions <- pupil |>
  filter(!Session %in% c("a1", "a2")) |>
  distinct(Session)

if (nrow(invalid_sessions) > 0) {
  write_error_and_stop(
    invalid_sessions,
    "ERROR_invalid_sessions.xlsx",
    "Unexpected session values found; expected only a1 and a2."
  )
}

duplicate_pupil <- pupil |>
  count(project_std, id_std, Session, name = "n") |>
  filter(n > 1)

if (nrow(duplicate_pupil) > 0) {
  warning(
    "Duplicate pupil rows were found. Earlier rows will be removed and only ",
    "the last row per project, ID, and session will be retained.",
    call. = FALSE
  )

  pupil <- pupil |>
    mutate(.original_row = row_number())

  removed_pupil_duplicates <- pupil |>
    group_by(project_std, id_std, Session) |>
    filter(n() > 1, row_number() < n()) |>
    ungroup()

  write_xlsx_table(
    removed_pupil_duplicates,
    file.path(dir_tables, "duplicates_pupil_removed.xlsx"),
    sheet_name = "removed_duplicates"
  )

  # Temporary rule: retain only the last row in original file order.
  pupil <- pupil |>
    group_by(project_std, id_std, Session) |>
    slice_tail(n = 1) |>
    ungroup() |>
    arrange(.original_row) |>
    select(-.original_row)
}

duplicate_clinical <- clinical |>
  count(project_std, id_std, name = "n") |>
  filter(n > 1)

if (nrow(duplicate_clinical) > 0) {
  warning(
    "Duplicate clinical rows were found. Earlier rows will be removed and only ",
    "the last row per project and ID will be retained.",
    call. = FALSE
  )

  clinical <- clinical |>
    mutate(.original_row = row_number())

  removed_clinical_duplicates <- clinical |>
    group_by(project_std, id_std) |>
    filter(n() > 1, row_number() < n()) |>
    ungroup()

  write_xlsx_table(
    removed_clinical_duplicates,
    file.path(dir_tables, "duplicates_clinical_removed.xlsx"),
    sheet_name = "removed_duplicates"
  )

  # Temporary rule: retain only the last row in original file order.
  clinical <- clinical |>
    group_by(project_std, id_std) |>
    slice_tail(n = 1) |>
    ungroup() |>
    arrange(.original_row) |>
    select(-.original_row)
}


# ---- Match pupil and clinical data ------------------------------------------

clinical_analysis <- clinical |>
  select(
    id_std,
    project_std,
    any_of(c("vp_id", "sample", "age_years", "group", "gender")),
    all_of(clinical_vars)
  )

unmatched_pupil <- pupil |>
  distinct(project_std, id_std) |>
  anti_join(
    clinical_analysis |> distinct(project_std, id_std),
    by = c("project_std", "id_std")
  )

if (nrow(unmatched_pupil) > 0) {
  write_xlsx_table(
    unmatched_pupil,
    file.path(dir_tables, "WARNING_unmatched_pupil_ids_excluded.xlsx"),
    sheet_name = "unmatched_excluded"
  )

  warning(
    nrow(unmatched_pupil),
    " pupil participant(s) could not be matched to clinical data. ",
    "These participants will be excluded from all analyses. ",
    "See WARNING_unmatched_pupil_ids_excluded.xlsx.",
    call. = FALSE
  )

  pupil <- pupil |>
    anti_join(
      unmatched_pupil,
      by = c("project_std", "id_std")
    )
}

clinical_without_pupil <- clinical_analysis |>
  distinct(project_std, id_std) |>
  anti_join(
    pupil |> distinct(project_std, id_std),
    by = c("project_std", "id_std")
  )

write_xlsx_table(
  clinical_without_pupil,
  file.path(dir_tables, "clinical_participants_without_pupil_data.xlsx"),
  sheet_name = "clinical_without_pupil"
)

matched_session <- pupil |>
  select(
    SubjectID,
    id_std,
    project_std,
    Session,
    all_of(distribution_pupil_vars)
  ) |>
  left_join(
    clinical_analysis,
    by = c("project_std", "id_std")
  )

check_missing(
  matched_session,
  c("id_std", "project_std", "Session", distribution_pupil_vars, clinical_vars),
  "matched_session_data"
)

matched_average <- matched_session |>
  group_by(project_std, id_std) |>
  summarise(
    across(all_of(distribution_pupil_vars), mean),
    across(
      any_of(c(
        "SubjectID", "vp_id", "sample", "age_years", "group", "gender"
      )),
      first
    ),
    across(all_of(clinical_vars), first),
    .groups = "drop"
  )

analysis_data <- list(
  average_a1_a2 = matched_average,
  session_a1 = matched_session |> filter(Session == "a1"),
  session_a2 = matched_session |> filter(Session == "a2")
)

iwalk(
  analysis_data,
  ~ {
    check_missing(
      .x,
      c("id_std", "project_std", correlation_pupil_vars, clinical_vars),
      paste0("analysis_", .y)
    )

    check_variance(
      .x,
      c(correlation_pupil_vars, clinical_vars),
      paste0("analysis_", .y)
    )
  }
)

write_xlsx_table(
  matched_session,
  file.path(dir_data, "matched_session_level_data.xlsx"),
  sheet_name = "matched_session"
)

write_xlsx_table(
  matched_average,
  file.path(dir_data, "matched_average_across_a1_a2_data.xlsx"),
  sheet_name = "matched_average"
)


# ---- Variable dictionary ----------------------------------------------------

pupil_dictionary <- tibble(
  pupil_measure = correlation_pupil_vars,
  pupil_id = "P01",
  pupil_label = "Baseline arousal"
)

distribution_pupil_dictionary <- tibble(
  pupil_measure = distribution_pupil_vars,
  pupil_label = c(
    "Baseline arousal",
    "Raw black dilation",
    "Raw white dilation",
    "Raw gray dilation"
  )
)

clinical_dictionary <- tibble(
  clinical_measure = clinical_vars,
  clinical_id = sprintf("C%02d", seq_along(clinical_vars)),
  clinical_label = clinical_vars |>
    str_remove("^score_") |>
    str_replace_all("__", ": ") |>
    str_replace_all("_", " ") |>
    str_to_sentence()
)


# ---- Pearson correlations ---------------------------------------------------

correlation_results <- imap_dfr(
  analysis_data,
  function(data, analysis_name) {
    crossing(
      pupil_measure = correlation_pupil_vars,
      clinical_measure = clinical_vars
    ) |>
      pmap_dfr(
        ~ run_correlation(
          data = data,
          analysis_set = analysis_name,
          pupil_var = ..1,
          clinical_var = ..2
        )
      )
  }
) |>
  group_by(analysis_set) |>
  mutate(
    p_fdr_within_analysis = p.adjust(p_value, method = "BH"),
    p_bonferroni_within_analysis = p.adjust(
      p_value,
      method = "bonferroni"
    )
  ) |>
  group_by(analysis_set, pupil_measure) |>
  mutate(
    p_fdr_within_pupil_measure = p.adjust(p_value, method = "BH")
  ) |>
  ungroup() |>
  mutate(
    p_fdr_global = p.adjust(p_value, method = "BH"),
    p_bonferroni_global = p.adjust(p_value, method = "bonferroni"),
    significant_uncorrected = p_value < alpha,
    significant_fdr_global = p_fdr_global < alpha,
    significant_fdr_within_analysis = p_fdr_within_analysis < alpha,
    significant_fdr_within_pupil =
      p_fdr_within_pupil_measure < alpha
  ) |>
  left_join(pupil_dictionary, by = "pupil_measure") |>
  left_join(clinical_dictionary, by = "clinical_measure") |>
  arrange(analysis_set, pupil_id, p_value, clinical_id) |>
  mutate(plot_filename = paste0(pupil_id, "_", clinical_id, ".png")) |>
  select(
    analysis_set,
    pupil_id,
    pupil_measure,
    pupil_label,
    clinical_id,
    clinical_measure,
    clinical_label,
    n_participants,
    r,
    ci_low_95,
    ci_high_95,
    r_squared,
    effect_magnitude,
    direction,
    t_value,
    degrees_of_freedom,
    p_value,
    p_fdr_global,
    p_bonferroni_global,
    p_fdr_within_analysis,
    p_bonferroni_within_analysis,
    p_fdr_within_pupil_measure,
    significant_uncorrected,
    significant_fdr_global,
    significant_fdr_within_analysis,
    significant_fdr_within_pupil,
    pupil_mean,
    pupil_sd,
    clinical_mean,
    clinical_sd,
    plot_filename
  )

write_xlsx_table(
  correlation_results,
  file.path(dir_tables, "correlation_results_all.xlsx"),
  sheet_name = "correlations_all"
)

write_xlsx_table(
  correlation_results |> filter(significant_uncorrected),
  file.path(dir_tables, "correlation_results_p_lt_05_uncorrected.xlsx"),
  sheet_name = "p_lt_05"
)

write_xlsx_table(
  correlation_results |> filter(significant_fdr_global),
  file.path(dir_tables, "correlation_results_FDR_global_significant.xlsx"),
  sheet_name = "FDR_significant"
)


# ---- Descriptive statistics -------------------------------------------------

pupil_session_long <- matched_session |>
  pivot_longer(
    all_of(distribution_pupil_vars),
    names_to = "pupil_measure",
    values_to = "value"
  ) |>
  left_join(distribution_pupil_dictionary, by = "pupil_measure")

pupil_average_long <- matched_average |>
  pivot_longer(
    all_of(distribution_pupil_vars),
    names_to = "pupil_measure",
    values_to = "value"
  ) |>
  left_join(distribution_pupil_dictionary, by = "pupil_measure")

describe_pupil <- function(data, grouping) {
  data |>
    group_by(across(all_of(grouping))) |>
    summarise(
      n = n(),
      n_participants = n_distinct(id_std),
      mean = mean(value),
      sd = sd(value),
      median = median(value),
      q1 = quantile(value, 0.25),
      q3 = quantile(value, 0.75),
      iqr = IQR(value),
      min = min(value),
      max = max(value),
      .groups = "drop"
    )
}

desc_session <- describe_pupil(
  pupil_session_long,
  c("Session", "pupil_measure", "pupil_label")
)

desc_project_session <- describe_pupil(
  pupil_session_long,
  c("project_std", "Session", "pupil_measure", "pupil_label")
)

desc_average <- describe_pupil(
  pupil_average_long,
  c("pupil_measure", "pupil_label")
)

# Text labels that are interpreted as female. The matching is
# case-insensitive and also detects labels such as "2 - weiblich".
female_gender_labels <- c(
  "female",
  "weiblich",
  "woman",
  "frau",
  "f",
  "w"
)

is_female_gender <- function(x) {
  gender_clean <- x |>
    as.character() |>
    str_squish() |>
    str_to_lower()

  exact_match <- gender_clean %in% female_gender_labels

  text_match <- str_detect(
    gender_clean,
    "(^|[^[:alpha:]])(female|weiblich|woman|frau)([^[:alpha:]]|$)"
  )

  !is.na(gender_clean) & (exact_match | text_match)
}

summarise_sample <- function(data) {
  gender_clean <- data$gender |>
    as.character() |>
    str_squish()

  gender_available <- !is.na(gender_clean) & gender_clean != ""
  female <- is_female_gender(data$gender)

  age_available <- !is.na(data$age_years)

  tibble(
    n_participants = n_distinct(data$id_std),
    n_with_age = sum(age_available),
    age_mean = if_else(
      any(age_available),
      mean(data$age_years[age_available]),
      NA_real_
    ),
    age_sd = if_else(
      sum(age_available) >= 2,
      sd(data$age_years[age_available]),
      NA_real_
    ),
    age_min = if_else(
      any(age_available),
      min(data$age_years[age_available]),
      NA_real_
    ),
    age_max = if_else(
      any(age_available),
      max(data$age_years[age_available]),
      NA_real_
    ),
    age_range = if_else(
      any(age_available),
      paste0(
        formatC(
          min(data$age_years[age_available]),
          format = "f",
          digits = 1
        ),
        "–",
        formatC(
          max(data$age_years[age_available]),
          format = "f",
          digits = 1
        )
      ),
      NA_character_
    ),
    n_with_gender = sum(gender_available),
    n_female = sum(female & gender_available),
    proportion_female = if_else(
      any(gender_available),
      sum(female & gender_available) / sum(gender_available),
      NA_real_
    ),
    percent_female = if_else(
      any(gender_available),
      100 * sum(female & gender_available) / sum(gender_available),
      NA_real_
    ),
    gender_values_present = paste(
      sort(unique(gender_clean[gender_available])),
      collapse = "; "
    )
  )
}

sample_summary <- imap_dfr(
  analysis_data,
  function(data, analysis_name) {
    by_project <- data |>
      group_by(project_std) |>
      group_modify(~ summarise_sample(.x)) |>
      ungroup()

    all_projects <- summarise_sample(data) |>
      mutate(project_std = "ALL_PROJECTS", .before = 1)

    bind_rows(by_project, all_projects) |>
      mutate(analysis_set = analysis_name, .before = 1)
  }
) |>
  arrange(analysis_set, project_std)

write_xlsx_table(
  desc_session,
  file.path(dir_tables, "pupil_descriptives_by_session.xlsx"),
  sheet_name = "descriptives"
)

write_xlsx_table(
  desc_project_session,
  file.path(dir_tables, "pupil_descriptives_by_project_and_session.xlsx"),
  sheet_name = "descriptives"
)

write_xlsx_table(
  desc_average,
  file.path(dir_tables, "pupil_descriptives_average_a1_a2.xlsx"),
  sheet_name = "descriptives"
)

write_xlsx_table(
  sample_summary,
  file.path(dir_tables, "sample_summary.xlsx"),
  sheet_name = "sample_summary"
)


# ---- Pupil-distribution plots -----------------------------------------------

plot_session <- ggplot(
  pupil_session_long,
  aes(Session, value)
) +
  geom_violin(trim = FALSE, alpha = 0.35) +
  geom_boxplot(width = 0.13, outlier.shape = NA, alpha = 0.55) +
  geom_jitter(width = 0.08, alpha = 0.35, size = 1.2) +
  facet_wrap(~ pupil_label, scales = "free_y", ncol = 2) +
  labs(
    title = "Pupil distributions by session",
    x = "Session",
    y = "Value"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

plot_average <- ggplot(
  pupil_average_long,
  aes("Mean across a1 and a2", value)
) +
  geom_violin(trim = FALSE, alpha = 0.35) +
  geom_boxplot(width = 0.13, outlier.shape = NA, alpha = 0.55) +
  geom_jitter(width = 0.08, alpha = 0.35, size = 1.2) +
  facet_wrap(~ pupil_label, scales = "free_y", ncol = 2) +
  labs(
    title = "Pupil distributions averaged across a1 and a2",
    x = NULL,
    y = "Across-session mean"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

session_colors <- c(
  a1 = "#183A4A",
  a2 = "#6A2438"
)

plot_project <- ggplot(
  pupil_session_long,
  aes(
    x = project_std,
    y = value,
    fill = Session,
    color = Session
  )
) +
  geom_violin(
    aes(group = interaction(project_std, Session)),
    position = position_dodge(width = 0.90),
    width = 0.82,
    trim = FALSE,
    alpha = 0.25
  ) +
  geom_boxplot(
    aes(group = interaction(project_std, Session)),
    position = position_dodge(width = 0.90),
    width = 0.16,
    outlier.shape = NA,
    alpha = 0.45
  ) +
  geom_point(
    aes(shape = Session),
    position = position_jitterdodge(
      jitter.width = 0.07,
      jitter.height = 0,
      dodge.width = 0.90
    ),
    alpha = 0.40,
    size = 1.1
  ) +
  scale_fill_manual(
    values = session_colors,
    name = "Session"
  ) +
  scale_color_manual(
    values = session_colors,
    name = "Session"
  ) +
  scale_shape_manual(
    values = c(a1 = 16, a2 = 17),
    name = "Session"
  ) +
  facet_wrap(~ pupil_label, scales = "free_y", ncol = 2) +
  labs(
    title = "Observed pupil distributions by project × session",
    x = "Project",
    y = "Value"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

distribution_plots <- list(
  pupil_distributions_by_session = plot_session,
  pupil_distributions_average_a1_a2 = plot_average,
  pupil_distributions_by_project_and_session = plot_project
)

iwalk(
  distribution_plots,
  ~ {
    ggsave(
      file.path(dir_distributions, paste0(.y, ".png")),
      .x,
      width = 11,
      height = 8,
      dpi = 300
    )

    ggsave(
      file.path(dir_distributions, paste0(.y, ".pdf")),
      .x,
      width = 11,
      height = 8
    )
  }
)


# ---- Project × session mixed models -----------------------------------------

# A mixed model is used rather than an ordinary lm because a1 and a2 are
# repeated observations from the same participant.
#
# Fixed effects:
#   project + session + project × session
#
# Random effect:
#   participant-specific intercept

model_session_data <- pupil_session_long |>
  mutate(
    project_std = factor(project_std),
    Session = factor(Session, levels = c("a1", "a2")),
    id_std = factor(id_std)
  )

fit_project_session_model <- function(data, pupil_measure_name) {
  model_data <- data |>
    filter(pupil_measure == pupil_measure_name)

  model <- lmerTest::lmer(
    value ~ project_std * Session + (1 | id_std),
    data = model_data,
    REML = TRUE
  )

  fixed_effects <- summary(model)$coefficients |>
    as.data.frame() |>
    tibble::rownames_to_column("term") |>
    as_tibble() |>
    transmute(
      pupil_measure = pupil_measure_name,
      term = term,
      estimate = Estimate,
      std_error = `Std. Error`,
      degrees_of_freedom = df,
      t_value = `t value`,
      p_value = `Pr(>|t|)`
    )

  omnibus_tests <- stats::anova(
    model,
    type = 3,
    ddf = "Satterthwaite"
  ) |>
    as.data.frame() |>
    tibble::rownames_to_column("effect") |>
    as_tibble() |>
    transmute(
      pupil_measure = pupil_measure_name,
      effect = effect,
      numerator_df = NumDF,
      denominator_df = DenDF,
      f_value = `F value`,
      p_value = `Pr(>F)`
    )

  estimated_means_object <- emmeans::emmeans(
    model,
    ~ project_std * Session
  )

  estimated_means <- summary(
    estimated_means_object,
    infer = c(TRUE, TRUE)
  ) |>
    as.data.frame() |>
    as_tibble() |>
    mutate(
      pupil_measure = pupil_measure_name,
      .before = 1
    )

  session_contrasts <- emmeans::contrast(
    estimated_means_object,
    method = "pairwise",
    by = "project_std",
    adjust = "holm"
  ) |>
    summary(infer = c(TRUE, TRUE)) |>
    as.data.frame() |>
    as_tibble() |>
    mutate(
      pupil_measure = pupil_measure_name,
      contrast_type = "session within project",
      .before = 1
    )

  project_contrasts <- emmeans::contrast(
    estimated_means_object,
    method = "pairwise",
    by = "Session",
    adjust = "holm"
  ) |>
    summary(infer = c(TRUE, TRUE)) |>
    as.data.frame() |>
    as_tibble() |>
    mutate(
      pupil_measure = pupil_measure_name,
      contrast_type = "project within session",
      .before = 1
    )

  random_effects <- lme4::VarCorr(model) |>
    as.data.frame() |>
    as_tibble() |>
    transmute(
      pupil_measure = pupil_measure_name,
      grouping_factor = grp,
      term_1 = var1,
      term_2 = var2,
      variance = vcov,
      standard_deviation = sdcor
    )

  model_fit <- tibble(
    pupil_measure = pupil_measure_name,
    n_observations = nobs(model),
    n_participants = n_distinct(model_data$id_std),
    n_projects = n_distinct(model_data$project_std),
    AIC = AIC(model),
    BIC = BIC(model),
    log_likelihood = as.numeric(logLik(model)),
    REML_criterion = deviance(model),
    singular_fit = lme4::isSingular(model, tol = 1e-4)
  )

  list(
    model = model,
    fixed_effects = fixed_effects,
    omnibus_tests = omnibus_tests,
    estimated_means = estimated_means,
    session_contrasts = session_contrasts,
    project_contrasts = project_contrasts,
    random_effects = random_effects,
    model_fit = model_fit
  )
}

project_session_models <- set_names(
  distribution_pupil_vars,
  distribution_pupil_vars
) |>
  map(
    ~ fit_project_session_model(
      model_session_data,
      .x
    )
  )

model_fixed_effects <- map_dfr(
  project_session_models,
  "fixed_effects"
)

model_omnibus_tests <- map_dfr(
  project_session_models,
  "omnibus_tests"
) |>
  mutate(
    p_fdr_across_all_tests = p.adjust(p_value, method = "BH")
  )

model_estimated_means <- map_dfr(
  project_session_models,
  "estimated_means"
) |>
  left_join(
    distribution_pupil_dictionary,
    by = "pupil_measure"
  )

model_session_contrasts <- map_dfr(
  project_session_models,
  "session_contrasts"
) |>
  group_by(pupil_measure) |>
  mutate(
    p_fdr_within_measure = p.adjust(p.value, method = "BH")
  ) |>
  ungroup()

model_project_contrasts <- map_dfr(
  project_session_models,
  "project_contrasts"
) |>
  group_by(pupil_measure) |>
  mutate(
    p_fdr_within_measure = p.adjust(p.value, method = "BH")
  ) |>
  ungroup()

model_random_effects <- map_dfr(
  project_session_models,
  "random_effects"
)

model_fit_summary <- map_dfr(
  project_session_models,
  "model_fit"
)

interaction_plot <- ggplot(
  model_estimated_means,
  aes(
    x = project_std,
    y = emmean,
    group = Session,
    color = Session,
    shape = Session
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(
      ymin = lower.CL,
      ymax = upper.CL
    ),
    width = 0.10,
    linewidth = 0.7
  ) +
  scale_color_manual(
    values = session_colors,
    name = "Session"
  ) +
  scale_shape_manual(
    values = c(a1 = 16, a2 = 17),
    name = "Session"
  ) +
  facet_wrap(
    ~ pupil_label,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    title = "Model-estimated project × session interactions",
    subtitle = paste0(
      "Linear mixed models with participant-specific random intercepts; ",
      "points show estimated marginal means and bars show 95% CIs"
    ),
    x = "Project",
    y = "Estimated marginal mean"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave(
  file.path(
    dir_interactions,
    "model_estimated_project_x_session_interactions.png"
  ),
  interaction_plot,
  width = 11,
  height = 8,
  dpi = 300
)

ggsave(
  file.path(
    dir_interactions,
    "model_estimated_project_x_session_interactions.pdf"
  ),
  interaction_plot,
  width = 11,
  height = 8
)

model_excel_file <- file.path(
  dir_tables,
  "pupil_project_x_session_mixed_models.xlsx"
)

model_workbook <- createWorkbook()

model_sheets <- list(
  omnibus_tests = model_omnibus_tests,
  fixed_effects = model_fixed_effects,
  estimated_means = model_estimated_means,
  session_contrasts = model_session_contrasts,
  project_contrasts = model_project_contrasts,
  random_effects = model_random_effects,
  model_fit = model_fit_summary
)

iwalk(
  model_sheets,
  ~ {
    addWorksheet(model_workbook, .y)
    writeData(
      model_workbook,
      sheet = .y,
      x = .x,
      withFilter = nrow(.x) > 0
    )
    freezePane(
      model_workbook,
      sheet = .y,
      firstRow = TRUE
    )
    setColWidths(
      model_workbook,
      sheet = .y,
      cols = seq_len(ncol(.x)),
      widths = "auto"
    )
  }
)

saveWorkbook(
  model_workbook,
  model_excel_file,
  overwrite = TRUE
)


# ---- Scatterplots with regression line and 95% confidence band --------------

make_scatterplot <- function(data, result_row) {
  x_var <- result_row$clinical_measure[[1]]
  y_var <- result_row$pupil_measure[[1]]

  subtitle <- paste0(
    "Pearson r = ", sprintf("%.3f", result_row$r),
    " [", sprintf("%.3f", result_row$ci_low_95),
    ", ", sprintf("%.3f", result_row$ci_high_95),
    "], p ", format_p(result_row$p_value),
    ", global FDR p ", format_p(result_row$p_fdr_global),
    ", n = ", result_row$n_participants
  )

  project_levels <- data |>
    dplyr::pull(project_std) |>
    unique() |>
    sort()

  dark_project_colors <- grDevices::hcl.colors(
    n = length(project_levels),
    palette = "Dark 3"
  )

  names(dark_project_colors) <- project_levels

  ggplot(
    data,
    aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      color = project_std
    )
  ) +
    geom_point(alpha = 0.75, size = 2.2) +
    geom_smooth(
      aes(group = 1),
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      color = "black",
      linewidth = 0.9
    ) +
    scale_color_manual(
      values = dark_project_colors,
      name = "Project"
    ) +
    labs(
      title = paste0(
        result_row$pupil_label,
        " and ",
        result_row$clinical_label
      ),
      subtitle = subtitle,
      x = result_row$clinical_label,
      y = result_row$pupil_label,
      caption = paste0("Analysis set: ", result_row$analysis_set)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
}

save_scatterplots <- function(analysis_name) {
  data <- analysis_data[[analysis_name]]
  results <- correlation_results |>
    filter(analysis_set == analysis_name)

  analysis_dir <- file.path(dir_scatter, analysis_name)
  dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

  pdf(
    file.path(analysis_dir, paste0("all_scatterplots_", analysis_name, ".pdf")),
    width = 8.5,
    height = 6.5,
    onefile = TRUE
  )

  tryCatch(
    {
      for (i in seq_len(nrow(results))) {
        row <- results[i, ]
        plot <- make_scatterplot(data, row)
        print(plot)

        pupil_dir <- file.path(analysis_dir, row$pupil_id)
        dir.create(pupil_dir, recursive = TRUE, showWarnings = FALSE)

        ggsave(
          file.path(pupil_dir, row$plot_filename),
          plot,
          width = 8.5,
          height = 6.5,
          dpi = 300
        )
      }
    },
    finally = dev.off()
  )
}

walk(names(analysis_data), save_scatterplots)


# ---- Excel workbook ---------------------------------------------------------

excel_file <- file.path(
  dir_tables,
  "correlation_baseline_arousal_CAPE_IDAS_results.xlsx"
)

excel_sheets <- list(
  correlations_all = correlation_results,
  p_lt_05_uncorrected = correlation_results |>
    filter(significant_uncorrected),
  FDR_global_significant = correlation_results |>
    filter(significant_fdr_global),
  pupil_desc_session = desc_session,
  pupil_desc_project = desc_project_session,
  pupil_desc_average = desc_average,
  sample_summary = sample_summary,
  model_omnibus = model_omnibus_tests,
  model_fixed = model_fixed_effects,
  model_emmeans = model_estimated_means,
  session_contrasts = model_session_contrasts,
  project_contrasts = model_project_contrasts,
  model_fit = model_fit_summary,
  pupil_dictionary = pupil_dictionary,
  clinical_dictionary = clinical_dictionary
)

workbook <- createWorkbook()

iwalk(
  excel_sheets,
  ~ {
    addWorksheet(workbook, .y)
    writeData(
      workbook,
      sheet = .y,
      x = .x,
      withFilter = nrow(.x) > 0
    )
    freezePane(workbook, sheet = .y, firstRow = TRUE)
    setColWidths(
      workbook,
      sheet = .y,
      cols = seq_len(ncol(.x)),
      widths = "auto"
    )
  }
)

saveWorkbook(workbook, excel_file, overwrite = TRUE)


# ---- Reproducibility information --------------------------------------------

analysis_notes <- tibble(
  information = c(
    "Analysis sets:",
    "average_a1_a2 = participant means across a1 and a2",
    "session_a1 = a1 only",
    "session_a2 = a2 only",
    "",
    "Pearson correlations use BaselineArousal only.",
    "RawBlackDilation, RawWhiteDilation, and RawGrayDilation are used only",
    "for descriptive distribution plots and are not correlated.",
    "Correlations are pooled across projects.",
    "Project × session effects are additionally tested using linear mixed models.",
    "Model formula: pupil value ~ project * session + (1 | participant).",
    "Reported corrections: global FDR, global Bonferroni,",
    "FDR/Bonferroni within analysis set, and FDR within pupil measure.",
    "",
    paste0("Created: ", Sys.time())
  )
)

r_session_info <- tibble(
  session_info = capture.output(sessionInfo())
)

metadata_workbook <- createWorkbook()

addWorksheet(metadata_workbook, "analysis_notes")
writeData(
  metadata_workbook,
  sheet = "analysis_notes",
  x = analysis_notes
)
setColWidths(
  metadata_workbook,
  sheet = "analysis_notes",
  cols = 1,
  widths = 90
)

addWorksheet(metadata_workbook, "R_session_info")
writeData(
  metadata_workbook,
  sheet = "R_session_info",
  x = r_session_info
)
setColWidths(
  metadata_workbook,
  sheet = "R_session_info",
  cols = 1,
  widths = 120
)

saveWorkbook(
  metadata_workbook,
  file.path(output_dir, "analysis_metadata.xlsx"),
  overwrite = TRUE
)

message("Analysis completed.")
message("Main workbook: ", excel_file)
message("All output: ", output_dir)
