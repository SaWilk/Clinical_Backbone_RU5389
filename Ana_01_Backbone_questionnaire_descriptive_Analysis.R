#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FOR: Separate Backbone Data by Project — Cleaning & Export Pipeline
# Authors: Saskia Wilken (saskia.wilken@uni-hamburg.de, saskia.a.wilken@gmail.com))
# First edited: 2025-11-07 (SW)
#
# Description:
# Reads questionnaire data, filters bad rows, reverse-codes, exports per project,
# produces simple demographics plots (age density, education overlay, % women),
# writes sanity logs, and keeps keys.
# (Legacy helpers kept but not called.)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ---- Setup -------------------------------------------------------------------

ensure_packages <- function(pkgs) {
  to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(to_install)) install.packages(to_install, quiet = TRUE)
  invisible(lapply(pkgs, require, character.only = TRUE))
}

ensure_packages(c(
  "readxl", "writexl", "janitor", "stringr", "dplyr", "tidyr",
  "purrr", "lubridate", "tibble", "glue", "fs", "jsonlite", "readr",
  "ggplot2", "forcats", "cluster", "tidyselect"
))

# ---- Console clear -----------------------------------------------------------
clear_console <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    cat("\014"); return(invisible(TRUE))
  }
  if (.Platform$OS.type == "windows") {
    try(system("cls", intern = TRUE), silent = TRUE)
  } else {
    try(system("clear", intern = TRUE), silent = TRUE)
  }
  invisible(TRUE)
}
# clear immediately when sourcing
clear_console()

# Project colors (as provided). We'll ignore the "* children" entries when plotting.
vanilla_colors <- c(
  "p2"="#1B9E77","p3"="#D95F02","p4"="#7570B3", "p5"="#E7298A",
  "p6 children"="#66A61E","p7"="#E6AB02", "p8 children"="#000000",
  "p8 adults" ="#A6761D", "p9"="#7A7A7A"
)

# ---- Paths & Logging ---------------------------------------------------------

script_dir <- function() {
  if (!interactive()) {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- "--file="
    filepath <- sub(file_arg, "", args[grep(file_arg, args)])
    if (length(filepath) == 1) return(normalizePath(dirname(filepath), winslash = "/"))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(normalizePath(dirname(p), winslash = "/"))
  }
  return(normalizePath(getwd(), winslash = "/"))
}

ROOT <- script_dir()

DIR_QUESTIONNAIRES <- fs::path(ROOT, "01_project_data", "all_projects_backbone", "questionnaires")
DIR_INFO            <- fs::path(ROOT, "information")
DIR_EXPORT          <- fs::path(ROOT, "02_cleaned")
DIR_KEYS            <- fs::path(DIR_EXPORT, "keys")
DIR_LOGS            <- fs::path(ROOT, "logs")
DIR_PRIVATE         <- fs::path(ROOT, "private_information")
DIR_ANALYSIS_BASE   <- fs::path(ROOT, "out", "internal_data_analysis", "demographics_and_health")
SANITY_LOG_FILE     <- fs::path(DIR_LOGS, "sanity_check_backbone_data_log.txt")

fs::dir_create(DIR_EXPORT)
fs::dir_create(DIR_KEYS)
fs::dir_create(DIR_LOGS)
fs::dir_create(DIR_ANALYSIS_BASE)

logfile <- fs::path(DIR_LOGS, glue::glue("clean_questionnaires_{format(Sys.time(), '%Y-%m-%d_%H%M%S')}.log"))

log_msg <- function(..., .sep = "", .newline = TRUE) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(..., collapse = .sep))
  if (.newline) msg <- paste0(msg, "\n")
  cat(msg)
  cat(msg, file = logfile, append = TRUE)
}

# ---- Filename helpers --------------------------------------------------------

normalize_sample_case <- function(sample) {
  stringr::str_replace_all(stringr::str_to_title(sample), "_", " ")
}

extract_date <- function(x) {
  d <- stringr::str_match(x, "(\\d{4}-\\d{2}-\\d{2})")[,2]
  suppressWarnings(lubridate::ymd(d))
}

extract_date_string <- function(path) {
  b <- basename(path)
  d <- stringr::str_match(b, "ALL_(\\d{4}-\\d{2}-\\d{2})_")[,2]
  ifelse(is.na(d), format(Sys.Date(), "%Y-%m-%d"), d)
}

latest_file_by_pattern <- function(dir, pattern) {
  files_all <- fs::dir_ls(dir, type = "file", fail = FALSE)
  files <- files_all[grepl(pattern, basename(files_all))]
  if (!length(files)) return(NA_character_)
  dts <- extract_date(basename(files))
  files <- files[order(dts, decreasing = TRUE)]
  files[1]
}

latest_questionnaire_for_sample <- function(sample) {
  patt <- glue::glue("^ALL_\\d{{4}}-\\d{{2}}-\\d{{2}}_{sample}_questionnaire\\.xlsx$")
  latest_file_by_pattern(DIR_QUESTIONNAIRES, patt)
}

latest_iteminfo_for_sample <- function(sample) {
  SampleCap <- normalize_sample_case(sample)
  patt <- glue::glue("^\\d{{4}}-\\d{{2}}-\\d{{2}}_Item_Information_{SampleCap}\\.xlsx$")
  latest_file_by_pattern(DIR_INFO, patt)
}

latest_scoring <- function() {
  patt <- "^\\d{4}-\\d{2}-\\d{2}_Scoring\\.xlsx$"
  latest_file_by_pattern(DIR_INFO, patt)
}

latest_groupings_file <- function() {
  patt <- "^\\d{4}-\\d{2}-\\d{2}_groupings\\.xlsx$"
  latest_file_by_pattern(DIR_PRIVATE, patt)
}

# ---- Normalization helpers ---------------------------------------------------

normalize_id <- function(x) {
  x %>% as.character() %>% stringr::str_to_lower() %>% stringr::str_replace_all("[^a-z0-9]", "")
}

normalize_vpid <- function(x) {
  x %>% as.character() %>% stringr::str_to_lower() %>% stringr::str_replace_all("[^a-z0-9]", "")
}

# ---- Project column detection ------------------------------------------------
get_project_col <- function(df) {
  if ("project" %in% names(df)) return("project")
  if ("p" %in% names(df)) return("p")
  return(NULL)
}

# ---- IO helpers --------------------------------------------------------------

read_questionnaire <- function(filepath) {
  stopifnot(is.character(filepath), length(filepath) == 1, fs::file_exists(filepath))
  log_msg("Reading questionnaire: ", filepath)
  df <- suppressMessages(readxl::read_excel(filepath))
  df <- janitor::remove_empty(df, which = c("rows", "cols"))
  tibble::as_tibble(df)
}

read_item_info <- function(filepath) {
  if (is.null(filepath) || is.na(filepath)) {
    log_msg("Item Information file not found for this sample — continuing without it.")
    return(NULL)
  }
  stopifnot(is.character(filepath), length(filepath) == 1, fs::file_exists(filepath))
  log_msg("Reading Item Information: ", filepath)
  suppressMessages(readxl::read_excel(filepath)) |>
    janitor::clean_names() |>
    dplyr::mutate(
      item_norm = normalize_id(.data$item),
      reverse_coded = as.logical(.data$reverse_coded)
    )
}

read_scoring <- function(filepath) {
  stopifnot(is.character(filepath), length(filepath) == 1, fs::file_exists(filepath))
  log_msg("Reading Scoring file: ", filepath)
  suppressMessages(readxl::read_excel(filepath)) |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(c(min, max), as.numeric))
}

# ---- Groupings helpers -------------------------------------------------------

read_groupings_for_sample <- function(sample, filepath = NULL) {
  if (is.null(filepath)) filepath <- latest_groupings_file()
  if (is.null(filepath) || is.na(filepath) || !fs::file_exists(filepath)) {
    log_msg("No groupings file found — skipping group mapping for sample '", sample, "'.")
    return(NULL)
  }
  log_msg("Reading groupings workbook: ", filepath)
  
  sheets <- readxl::excel_sheets(filepath)
  rx <- glue::glue("^p[^_]+_{sample}$")
  keep <- sheets[grepl(rx, sheets)]
  if (!length(keep)) {
    log_msg("No sheets matching sample '", sample, "' in groupings file — skipping.")
    return(NULL)
  }
  
  standardize_group <- function(x) {
    x0 <- x %>% as.character() %>% stringr::str_trim() %>% stringr::str_to_lower()
    dplyr::case_when(
      x0 %in% c("hc", "healthy", "control", "healthy control", "healthy_control", "healthy-control") ~ "HC",
      x0 %in% c("patient", "pt", "case", "clinical") ~ "patient",
      TRUE ~ NA_character_
    )
  }
  
  id_candidates <- c("vp","vpid","participant","participant_id","participantid","vp_id","vp-id")
  group_candidates <- c("group","grp","status","diagnosis","dx","health_status")
  
  out <- purrr::map_dfr(keep, function(sh) {
    df_sh <- suppressMessages(readxl::read_excel(filepath, sheet = sh)) %>%
      janitor::clean_names()
    
    # Mode A: explicit ID + group column
    id_col  <- intersect(names(df_sh), id_candidates) |> purrr::pluck(1, .default = NA_character_)
    grp_col <- intersect(names(df_sh), group_candidates) |> purrr::pluck(1, .default = NA_character_)
    
    if (!is.na(id_col) && !is.na(grp_col)) {
      p_val <- sub(glue::glue("^(p[^_]+)_{sample}$"), "\\1", sh)
      return(
        df_sh %>%
          dplyr::transmute(
            p = p_val,
            vp_join = normalize_vpid(.data[[id_col]]),
            group = standardize_group(.data[[grp_col]])
          ) %>%
          dplyr::filter(!is.na(vp_join) & !is.na(group))
      )
    }
    
    # Mode B: two columns "Patient"/"Control" with vpid values in rows
    pat_col <- intersect(names(df_sh), c("patient","patients")) |> purrr::pluck(1, .default = NA_character_)
    ctr_col <- intersect(names(df_sh), c("control","controls","hc")) |> purrr::pluck(1, .default = NA_character_)
    if (!is.na(pat_col) || !is.na(ctr_col)) {
      p_val <- sub(glue::glue("^(p[^_]+)_{sample}$"), "\\1", sh)
      rows_pat <- if (!is.na(pat_col)) tibble::tibble(vp_join = normalize_vpid(df_sh[[pat_col]]), group = "patient") else tibble::tibble()
      rows_ctr <- if (!is.na(ctr_col)) tibble::tibble(vp_join = normalize_vpid(df_sh[[ctr_col]]), group = "HC") else tibble::tibble()
      bind <- dplyr::bind_rows(rows_pat, rows_ctr) %>%
        dplyr::filter(!is.na(vp_join) & vp_join != "")
      if (nrow(bind)) bind$p <- p_val
      return(bind)
    }
    
    log_msg("Sheet '", sh, "' has no recognizable ID/group nor Patient/Control columns — skipped.")
    NULL
  })
  
  if (is.null(out) || nrow(out) == 0) {
    log_msg("Groupings for '", sample, "' contained no usable rows after cleaning.")
    return(NULL)
  }
  
  out <- dplyr::distinct(out, p, vp_join, .keep_all = TRUE)
  log_msg("Loaded ", nrow(out), " group mappings for sample '", sample, "'.")
  out
}

# NOTE: accept 'project' or 'p' without renaming
attach_groupings <- function(df, sample, id_guess = c("vp","vpid","participant","participant_id","participantid")) {
  proj_col <- get_project_col(df)
  if (is.null(proj_col)) {
    log_msg("No project column ('project' or 'p') in data — cannot attach groupings. Returning original df.")
    return(df)
  }
  grp <- read_groupings_for_sample(sample)
  if (is.null(grp)) return(df)
  
  id_in_df <- intersect(names(df), id_guess)
  if (!length(id_in_df)) {
    log_msg("No recognizable participant ID column in df — joining by project only (may be too coarse).")
    by_map <- setNames("p", proj_col)
    df2 <- df %>% dplyr::left_join(grp %>% dplyr::select(p, group) %>% dplyr::distinct(), by = by_map)
    return(df2)
  }
  
  id_col <- id_in_df[1]
  by_map <- setNames("p", proj_col)
  df %>%
    dplyr::mutate(.vp_join = normalize_vpid(.data[[id_col]])) %>%
    dplyr::left_join(grp, by = c(by_map, ".vp_join" = "vp_join")) %>%
    dplyr::select(-.vp_join)
}

# ---- Validation & keys -------------------------------------------------------

check_header_match <- function(q_df, item_info) {
  q_norm  <- normalize_id(names(q_df))
  ii_norm <- unique(item_info$item_norm)
  
  not_in_q  <- setdiff(ii_norm, q_norm)
  not_in_ii <- setdiff(q_norm, ii_norm)
  
  exempt <- c("p", "project", "rushingflag", "rushing_flag", "rushingmethod",
              "participant", "participantid", "vp", "vpid", "id", "submitdate",
              "startdate", "datestamp", "end", "consent", "startlanguage",
              "seed", "refurl", "lastpage")
  not_in_ii <- setdiff(not_in_ii, exempt)
  
  if (length(not_in_q) || length(not_in_ii)) {
    msg <- paste0(
      "⚠️  Header / Item Information mismatch detected.\n",
      if (length(not_in_q))
        paste0("  • Items expected but NOT found in questionnaire: ",
               paste(not_in_q, collapse = ", "), "\n")
      else "",
      if (length(not_in_ii))
        paste0("  • Columns in questionnaire NOT present in Item Information: ",
               paste(not_in_ii, collapse = ", "), "\n")
      else ""
    )
    warning(msg, call. = FALSE)
    log_msg(msg)
  } else {
    log_msg("Header check: questionnaire columns align with Item Information (filtered to scored scales).")
  }
  invisible(TRUE)
}

build_keys <- function(item_info) {
  ii <- item_info
  if (!"item_norm" %in% names(ii)) ii$item_norm <- normalize_id(ii$item)
  if (!"scale" %in% names(ii)) ii$scale <- NA_character_
  if (!"subscale" %in% names(ii)) ii$subscale <- NA_character_
  if (!"higher_order_subscale" %in% names(ii)) ii$higher_order_subscale <- NA_character_
  
  ii <- ii %>%
    dplyr::mutate(
      scale = as.character(scale),
      subscale = as.character(subscale),
      higher_order_subscale = as.character(higher_order_subscale)
    )
  
  items_by_scale <- ii %>% dplyr::group_by(scale) %>%
    dplyr::summarise(items = list(unique(item_norm)), .groups = "drop")
  
  items_by_sub <- ii %>% dplyr::group_by(scale, subscale) %>%
    dplyr::summarise(items = list(unique(item_norm)), .groups = "drop")
  
  items_by_ho <- ii %>% dplyr::group_by(higher_order_subscale) %>%
    dplyr::summarise(items = list(unique(item_norm)), .groups = "drop")
  
  leaf <- ii %>% dplyr::group_by(scale, higher_order_subscale, subscale) %>%
    dplyr::summarise(items = list(unique(item_norm)), .groups = "drop")
  
  ho_level <- leaf %>% dplyr::group_by(scale, higher_order_subscale) %>%
    dplyr::summarise(
      subscales = list(tibble::tibble(subscale = subscale, items = items)),
      .groups = "drop"
    )
  
  nested <- ho_level %>% dplyr::group_by(scale) %>%
    dplyr::summarise(
      higher_order = list(tibble::tibble(
        higher_order_subscale = higher_order_subscale,
        subscales = subscales
      )),
      .groups = "drop"
    )
  
  list(
    items_by_scale = items_by_scale,
    items_by_subscale = items_by_sub,
    items_by_higher_order = items_by_ho,
    nested = nested
  )
}

# ---- Row filtering -----------------------------------------------------------

remove_flagged_rows <- function(q_df, sample) {
  flag_col <- intersect(names(q_df), c("rushing_flag", "rushingflag")) |> purrr::pluck(1, .default = NA_character_)
  if (is.na(flag_col)) {
    log_msg("No rushing flag column found; 0 rows removed.")
    return(list(clean = q_df, discarded = tibble::tibble()))
  }
  flag_val <- q_df[[flag_col]]
  flag_log <- dplyr::case_when(
    is.logical(flag_val) ~ flag_val,
    is.numeric(flag_val) ~ flag_val != 0,
    TRUE ~ tolower(as.character(flag_val)) %in% c("true","t","1","yes","y")
  )
  discarded <- q_df %>% dplyr::filter(flag_log %in% TRUE)
  clean     <- q_df %>% dplyr::filter(!(flag_log %in% TRUE))
  log_msg(glue::glue("Sample '{sample}': removed {nrow(discarded)} rows due to rushing flag."))
  list(clean = clean, discarded = discarded)
}

# ---- Wide-only scoring helpers ----------------------------------------------

split_id_and_item_columns <- function(q_df, item_info, scoring) {
  valid_scales <- unique(scoring$scale)
  item_info_valid <- item_info %>%
    dplyr::filter(!is.na(scale) & scale != "" & .data$scale %in% valid_scales) %>%
    dplyr::mutate(item_norm = normalize_id(item))
  
  valid_ids <- unique(item_info_valid$item_norm)
  col_norm  <- normalize_id(names(q_df))
  is_item   <- col_norm %in% valid_ids
  
  id_cols   <- names(q_df)[!is_item]
  item_cols <- names(q_df)[is_item]
  
  log_msg("Using only items whose Scale is present in the Scoring sheet.")
  log_msg("Detected ", length(item_cols), " item columns and ",
          length(id_cols), " meta/ID columns for this sample.")
  
  list(
    id_cols   = id_cols,
    item_cols = item_cols,
    item_info_valid = item_info_valid
  )
}

reverse_code_wide <- function(df, item_cols, col_meta) {
  min_by_col <- col_meta$min; names(min_by_col) <- col_meta$orig
  max_by_col <- col_meta$max; names(max_by_col) <- col_meta$orig
  rev_by_col <- col_meta$reverse_coded; names(rev_by_col) <- col_meta$orig
  
  df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(item_cols),
        ~ suppressWarnings(as.numeric(.x))
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(item_cols),
        ~ {
          val <- .x
          col <- dplyr::cur_column()
          if (isTRUE(rev_by_col[[col]])) {
            min_by_col[[col]] + max_by_col[[col]] - val
          } else {
            val
          }
        }
      )
    )
}

# ---- Export helpers ----------------------------------------------------------

export_per_project <- function(df_clean, df_discard, sample) {
  out_dir <- fs::path(DIR_EXPORT, sample)
  fs::dir_create(out_dir)
  
  readr::write_csv(df_clean,   fs::path(out_dir, glue::glue("{sample}_clean_master.csv")))
  readr::write_csv(df_discard, fs::path(out_dir, glue::glue("{sample}_discarded.csv")))
  
  proj_col <- get_project_col(df_clean)
  if (is.null(proj_col)) {
    log_msg("No project column ('project' or 'p') — skipping per-project split.")
    return(invisible(NULL))
  }
  
  df_split <- split(df_clean, df_clean[[proj_col]])
  purrr::iwalk(df_split, function(dd, proj) {
    safe_proj <- gsub("[^A-Za-z0-9_-]+", "_", proj)
    filepath <- fs::path(out_dir, glue::glue("{sample}_project-{safe_proj}_clean.csv"))
    readr::write_csv(dd, filepath)
  })
  
  log_msg(glue::glue("Exported {length(df_split)} project-level files for sample '{sample}' (by column '{proj_col}')."))
}

save_keys <- function(keys, sample) {
  path_rds  <- fs::path(DIR_KEYS, glue::glue("{sample}_keys.rds"))
  path_json <- fs::path(DIR_KEYS, glue::glue("{sample}_keys.json"))
  saveRDS(keys, path_rds)
  jsonlite::write_json(keys, path_json, pretty = TRUE, auto_unbox = TRUE)
  log_msg(glue::glue("Saved keys for '{sample}' to: {path_rds} and {path_json}"))
}

# ---- Demographics & Health utilities -----------------------------------------

safe_var <- function(x) gsub("[^A-Za-z0-9_-]+", "_", x)

# FIXED: no tidyselect misuse; guard all columns safely
label_demographics_health <- function(df) {
  out <- df
  
  # Gender-like columns
  gender_cols <- intersect(
    c("gender",
      grep("^parentsgender|^siblingsgender|^cildrengender|^childrengender|^children(gender)?",
           names(out), value = TRUE, ignore.case = TRUE)),
    names(out)
  )
  if (length(gender_cols)) {
    out <- out %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(gender_cols),
          ~ factor(as.character(.x),
                   levels = c("1","2","3","0","-oth-"),
                   labels = c("female","male","nonbinary","no gender","other"))
        )
      )
  }
  
  # Education
  if ("education" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(
        education = factor(as.character(education),
                           levels = c("0","1","2","3","4","5","6","7","8","9","10","-oth-"),
                           labels = c("no school diploma",
                                      "primary school",
                                      "Lower Secondary School Certificate",
                                      "Intermediate Secondary School Certificate",
                                      "General Qualification for University Entrance",
                                      "apprentice",
                                      "applied science (Fachhochschulabschluss)",
                                      "diploma","bachelor","master","PhD","other"))
      ) %>%
      dplyr::mutate(
        education = forcats::fct_relevel(
          education,
          "no school diploma","primary school",
          "Lower Secondary School Certificate","Intermediate Secondary School Certificate",
          "General Qualification for University Entrance","apprentice",
          "applied science (Fachhochschulabschluss)",
          "bachelor","diploma","master","PhD","other"
        ),
        education = factor(education, levels = levels(education), ordered = TRUE)
      )
  }
  
  # Marital status
  if ("maritalstat" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(
        maritalstat = factor(as.character(maritalstat),
                             levels = c("0","1","2","3","4"),
                             labels = c("single","relationship","married","divorced","widowed"))
      )
  }
  
  # Eyesight / hearing (optional)
  sens_cols <- intersect(c("eyesight","hearing"), names(out))
  if (length(sens_cols)) {
    out <- out %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(sens_cols),
          ~ factor(as.character(.x),
                   levels = c("0","1","2"),
                   labels = c("normal","corrected","not corrected"))
        )
      )
  }
  
  # Binary health/contact flags (optional)
  bin_candidates <- c("psychomedication","othermedication","ownpsychdisorder")
  bin_candidates <- union(bin_candidates, grep("contact", names(out), value = TRUE))
  bin_cols <- intersect(bin_candidates, names(out))
  if (length(bin_cols)) {
    out <- out %>% dplyr::mutate(
      dplyr::across(dplyr::all_of(bin_cols),
                    ~ factor(as.character(.x), levels = c("0","1"), labels = c("no","yes")))
    )
  }
  
  out
}

# ---- Central enrichment (single source of truth) -----------------------------

enrich_demographics_and_health_fields <- function(df, sample) {
  d <- label_demographics_health(df)
  
  # Canonical age_years (DOB or numeric years)
  parse_date_safely <- function(x) suppressWarnings(as.Date(x))
  dob  <- if ("age" %in% names(d)) parse_date_safely(d$age) else as.Date(NA)
  subm <- if ("submitdate" %in% names(d)) parse_date_safely(d$submitdate) else as.Date(NA)
  age_num <- if ("age" %in% names(d)) suppressWarnings(as.numeric(d$age)) else rep(NA_real_, nrow(d))
  looks_like_years <- !all(is.na(age_num)) && stats::median(age_num, na.rm = TRUE) <= 120
  
  d <- d %>%
    dplyr::mutate(
      age_years = dplyr::case_when(
        looks_like_years ~ as.integer(floor(age_num)),
        !is.na(dob) & !is.na(subm) ~ as.integer(floor(lubridate::interval(dob, subm) / lubridate::years(1))),
        !is.na(dob) ~ as.integer(floor(lubridate::interval(dob, Sys.Date()) / lubridate::years(1))),
        TRUE ~ NA_integer_
      ),
      height = suppressWarnings(as.numeric(.data[["height"]])),
      weight = suppressWarnings(as.numeric(.data[["weight"]]))
    )
  
  # sensible thresholds + sanity log
  get_id_vec <- function(z) {
    if ("vpid" %in% names(z)) z$vpid
    else if ("vp" %in% names(z)) z$vp
    else if ("participantid" %in% names(z)) z$participantid
    else rep(NA_character_, nrow(z))
  }
  proj_col <- get_project_col(d)
  
  if (tolower(sample) %in% c("adults","adolescents")) {
    age_bad    <- !is.na(d$age_years) & d$age_years < 5L
    height_bad <- !is.na(d$height)    & d$height > 250
    weight_bad <- !is.na(d$weight)    & d$weight < 35
    
    rep_rows <- function(idx, var, values, rule) {
      if (any(idx, na.rm = TRUE)) {
        tibble::tibble(
          vpid = get_id_vec(d)[idx],
          p    = if (!is.null(proj_col)) d[[proj_col]][idx] else NA_character_,
          value = values[idx]
        ) %>% log_sanity(sample, ., var, rule)
      }
    }
    rep_rows(age_bad,    "age",    d$age_years, "below sensible threshold 5 years")
    rep_rows(height_bad, "height", d$height,    "above sensible threshold 250 cm")
    rep_rows(weight_bad, "weight", d$weight,    "below sensible threshold 35 kg")
    
    d <- d %>%
      dplyr::mutate(
        age_years = dplyr::if_else(age_bad,    as.integer(NA), age_years),
        height    = dplyr::if_else(height_bad, NA_real_,       height),
        weight    = dplyr::if_else(weight_bad, NA_real_,       weight)
      )
  }
  
  # minimal recodes used downstream
  if ("gender" %in% names(d)) {
    g <- as.character(d$gender)
    g[!(g %in% c("female","male"))] <- NA_character_
    d$gender <- factor(g, levels = c("female","male"))
  }
  if ("education" %in% names(d)) {
    e <- as.character(d$education)
    e[e %in% c("other","missing")] <- NA_character_
    lev <- setdiff(levels(d$education), c("other","missing"))
    d$education <- factor(e, levels = lev, ordered = TRUE)
  }
  if ("maritalstat" %in% names(d)) {
    # keep 'divorced' (we collapse to 'single' only in the plot)
    m <- as.character(d$maritalstat)
    ord <- c("single","relationship","married","divorced","widowed")
    d$maritalstat <- factor(m, levels = ord, ordered = TRUE)
  }
  
  d
}


# -------- Multiple-choice helpers (legacy; used by old plotting) --------------

pick_first_existing_col <- function(df, candidates) {
  for (nm in candidates) if (nm %in% names(df)) return(nm)
  return(NULL)
}

job_Y_count <- function(df, i) {
  nm <- pick_first_existing_col(df, c(
    glue::glue("job[{i}]"),
    glue::glue("job_{i}_"),
    glue::glue("job_{i}")
  ))
  if (is.null(nm)) return(0L)
  sum(df[[nm]] == "Y", na.rm = TRUE)
}

job_other_count <- function(df) {
  nm <- pick_first_existing_col(df, c("job[other]", "job_other"))
  if (is.null(nm)) return(0L)
  sum(!is.na(df[[nm]]))
}

compute_job_count <- function(df) {
  tibble::tibble(
    jobsearch = job_Y_count(df, 0),
    school = job_Y_count(df, 1),
    apprentice = job_Y_count(df, 2),
    university = job_Y_count(df, 3),
    self_employed = job_Y_count(df, 4),
    minijob = job_Y_count(df, 5),
    parttime = job_Y_count(df, 6),
    fulltime = job_Y_count(df, 7),
    home = job_Y_count(df, 8),
    support_institution = job_Y_count(df, 9),
    unable = job_Y_count(df, 10),
    retirement = job_Y_count(df, 11),
    other = job_other_count(df)
  ) %>%
    tidyr::pivot_longer(
      cols = c(jobsearch, school, apprentice, university, self_employed, minijob,
               parttime, fulltime, home, support_institution, unable, retirement, other),
      names_to = "level", values_to = "quantity"
    ) %>%
    dplyr::mutate(variable = "job")
}

diag_Y_count <- function(df, code) {
  nm <- pick_first_existing_col(df, c(
    glue::glue("ownpsychdiagn[{code}]"),
    glue::glue("ownpsychdiagn_{code}_"),
    glue::glue("ownpsychdiagn_{code}")
  ))
  if (is.null(nm)) return(0L)
  sum(df[[nm]] == "Y", na.rm = TRUE)
}

diag_other_count <- function(df) {
  nm <- pick_first_existing_col(df, c("ownpsychdiagn[other]", "ownpsychdiagn_other"))
  if (is.null(nm)) return(0L)
  sum(!is.na(df[[nm]]))
}

compute_diag_count <- function(df) {
  tibble::tibble(
    depression = diag_Y_count(df, "MDE"),
    bipolar    = diag_Y_count(df, "Bipolar"),
    OCD        = diag_Y_count(df, "OCD"),
    anxiety    = diag_Y_count(df, "Anx"),
    psychotic  = diag_Y_count(df, "Psy"),
    substance  = diag_Y_count(df, "SUD"),
    eatingdis  = diag_Y_count(df, "ED"),
    idk        = diag_Y_count(df, "idk"),
    other      = diag_other_count(df)
  ) %>%
    tidyr::pivot_longer(
      cols = c(depression, bipolar, OCD, anxiety, psychotic, substance,
               eatingdis, idk, other),
      names_to = "level", values_to = "quantity"
    ) %>%
    dplyr::mutate(variable = "diagnosis")
}

plot_mc_counts <- function(counts_tbl, title, out_file, N_total) {
  if (nrow(counts_tbl) == 0) return(invisible(NULL))
  dfp <- counts_tbl %>%
    dplyr::arrange(dplyr::desc(quantity)) %>%
    dplyr::mutate(pct = ifelse(N_total > 0, 100*quantity/N_total, NA_real_))
  p <- ggplot2::ggplot(dfp, ggplot2::aes(x = reorder(level, quantity), y = quantity)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(sprintf("%.1f", pct), "%")),
                       hjust = -0.1, size = 3) +
    ggplot2::labs(title = title, x = NULL, y = "Count") +
    ggplot2::expand_limits(y = max(dfp$quantity, na.rm = TRUE) * 1.12) +
    ggplot2::annotate("text", x = Inf, y = Inf,
                      label = paste0("N=", N_total),
                      hjust = 1.1, vjust = 1.5, size = 3)
  ggplot2::ggsave(out_file, plot = p, width = 7, height = 5, dpi = 150, limitsize = FALSE)
  log_msg("Saved: ", out_file)
  invisible(TRUE)
}

# Sanity logger ---------------------------------------------------------------

log_sanity <- function(sample, df_rows, var, rule_desc) {
  if (nrow(df_rows) == 0) return(invisible(NULL))
  cat("", file = SANITY_LOG_FILE, append = FALSE)
  apply(df_rows, 1, function(r) {
    msg <- glue::glue("🔺 vpid {r[['vpid']]} (p={r[['p']]}, sample={sample}) has {var}={r[['value']]} — {rule_desc}.")
    log_msg(msg)
    cat(paste0(msg, "\n"), file = SANITY_LOG_FILE, append = TRUE)
  })
  invisible(TRUE)
}

# ===================== SIMPLE PLOTS (USED) ====================================

percent_nonmissing <- function(x) {
  n <- length(x); nn <- sum(!is.na(x))
  if (n == 0) return(NA_real_)
  100 * nn / n
}

percent_nonmissing_report <- function(df_ready, proj_col) {
  vars <- c("age_years", "education", "gender")
  present <- intersect(vars, names(df_ready))
  if (!length(present)) return(invisible(NULL))
  log_msg("Non-missing coverage (%):")
  for (v in present) {
    p <- percent_nonmissing(df_ready[[v]])
    log_msg(glue::glue("  • {v}: {sprintf('%.1f', p)}% non-missing (overall)"))
  }
  if (!is.null(proj_col)) {
    split_df <- split(df_ready, df_ready[[proj_col]])
    for (proj in names(split_df)) {
      d <- split_df[[proj]]
      for (v in present) {
        p <- percent_nonmissing(d[[v]])
        log_msg(glue::glue("    - {proj}: {v}: {sprintf('%.1f', p)}% non-missing"))
      }
    }
  }
  invisible(TRUE)
}

plot_simple_demographics <- function(df_ready, sample, questionnaire_path) {
  proj_col <- get_project_col(df_ready)
  if (is.null(proj_col)) { log_msg("Simple plots skipped: no project column ('project' or 'p')."); return(invisible(NULL)) }
  date_str <- extract_date_string(questionnaire_path)
  out_dir  <- fs::path(DIR_ANALYSIS_BASE, paste0(date_str, "_", sample), "simple_plots")
  fs::dir_create(out_dir)
  
  to_palette_key <- function(x) {
    x_chr <- as.character(x)
    ifelse(x_chr == "8", "p8 adults", paste0("p", x_chr))
  }
  
  # 0) coverage
  percent_nonmissing_report(df_ready, proj_col)
  
  # 1) Age density overlay (percentage; alpha = 0.2)
  if ("age_years" %in% names(df_ready)) {
    d_age <- df_ready %>% dplyr::filter(!is.na(age_years), !is.na(.data[[proj_col]]))
    if (nrow(d_age)) {
      pal_keys <- to_palette_key(d_age[[proj_col]])
      pal <- vanilla_colors[unique(pal_keys)]; pal <- pal[!is.na(pal)]
      use_manual <- length(pal) > 0
      p_age <- ggplot2::ggplot(d_age, ggplot2::aes(x = age_years, color = pal_keys, fill = pal_keys)) +
        ggplot2::geom_density(ggplot2::aes(y = after_stat(density * 100)), alpha = 0.2, adjust = 1) +
        ggplot2::labs(title = paste0(sample, " — Age distribution by project"), x = "Age (years)", y = "Percentage") +
        { if (use_manual) ggplot2::scale_color_manual(values = pal, drop = FALSE) else ggplot2::scale_color_discrete() } +
        { if (use_manual) ggplot2::scale_fill_manual(values = pal, drop = FALSE) else ggplot2::scale_fill_discrete() } +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.title = ggplot2::element_blank())
      ggplot2::ggsave(fs::path(out_dir, "age_density_percentage.png"), p_age, width = 8, height = 5.2, dpi = 150)
      log_msg("Saved: ", fs::path(out_dir, "age_density_percentage.png"))
    } else { log_msg("Age plot: no complete rows (age_years + project).") }
  } else { log_msg("Age plot: 'age_years' not in data.") }
  
  # 2) Education "density" overlay (ordinal → integer ranks; alpha = 0.2)
  if ("education" %in% names(df_ready)) {
    d_edu <- df_ready %>% dplyr::filter(!is.na(education), !is.na(.data[[proj_col]]))
    if (nrow(d_edu)) {
      # drop two lowest categories
      drop_levels <- c("no school diploma", "primary school")
      d_edu <- d_edu %>%
        dplyr::mutate(education = droplevels(forcats::fct_drop(education, only = drop_levels))) %>%
        dplyr::filter(!is.na(education))
      if (nrow(d_edu)) {
        edu_levels <- levels(d_edu$education)
        d_edu <- d_edu %>% dplyr::mutate(edu_rank = as.integer(education))
        pal_keys <- to_palette_key(d_edu[[proj_col]])
        pal <- vanilla_colors[unique(pal_keys)]; pal <- pal[!is.na(pal)]
        use_manual <- length(pal) > 0
        
        p_edu_den <- ggplot2::ggplot(d_edu, ggplot2::aes(x = edu_rank, color = pal_keys, fill = pal_keys)) +
          ggplot2::geom_density(ggplot2::aes(y = after_stat(density * 100)), alpha = 0.2, adjust = 1) +
          ggplot2::scale_x_continuous(breaks = seq_along(edu_levels), labels = edu_levels,
                                      expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
          ggplot2::labs(title = paste0(sample, " — Education (ordinal) density by project"),
                        x = "Education", y = "Percentage") +
          { if (use_manual) ggplot2::scale_color_manual(values = pal, drop = FALSE) else ggplot2::scale_color_discrete() } +
          { if (use_manual) ggplot2::scale_fill_manual(values = pal, drop = FALSE) else ggplot2::scale_fill_discrete() } +
          ggplot2::theme_bw(base_size = 13) +
          ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_text(angle = 25, hjust = 1),
                         legend.title = ggplot2::element_blank())
        ggplot2::ggsave(fs::path(out_dir, "education_density_percentage.png"), p_edu_den, width = 9.5, height = 6.4, dpi = 150)
        log_msg("Saved: ", fs::path(out_dir, "education_density_percentage.png"))
      } else { log_msg("Education density: no rows after dropping low levels.") }
    } else { log_msg("Education density: no complete rows (education + project).") }
  } else { log_msg("Education density: 'education' not in data.") }
  
  # 3) % Women bars (solid)
  if ("gender" %in% names(df_ready)) {
    d_g <- df_ready %>% dplyr::filter(!is.na(.data[[proj_col]])) %>%
      dplyr::mutate(gender = as.character(gender)) %>%
      dplyr::filter(gender %in% c("female", "male"))
    if (nrow(d_g)) {
      women_pct <- d_g %>%
        dplyr::group_by(.data[[proj_col]]) %>%
        dplyr::summarise(N = dplyr::n(),
                         n_female = sum(gender == "female"),
                         pct_female = ifelse(N > 0, 100 * n_female / N, NA_real_),
                         .groups = "drop") %>%
        dplyr::mutate(palette_key = to_palette_key(.data[[proj_col]]))
      pal <- vanilla_colors[unique(women_pct$palette_key)]; pal <- pal[!is.na(pal)]
      use_manual <- length(pal) > 0
      women_pct[[proj_col]] <- factor(women_pct[[proj_col]], levels = women_pct[[proj_col]])
      p_w <- ggplot2::ggplot(women_pct, ggplot2::aes(x = .data[[proj_col]], y = pct_female, fill = palette_key)) +
        ggplot2::geom_col(width = 0.75) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", pct_female)), vjust = -0.3, size = 3) +
        ggplot2::labs(title = paste0(sample, " — % Women by project"), x = "Project", y = "Percentage women") +
        { if (use_manual) ggplot2::scale_fill_manual(values = pal, drop = FALSE) else ggplot2::scale_fill_discrete() } +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "none")
      ggplot2::ggsave(fs::path(out_dir, "women_percentage_by_project.png"), p_w, width = 8, height = 5.2, dpi = 150)
      log_msg("Saved: ", fs::path(out_dir, "women_percentage_by_project.png"))
    } else { log_msg("Women plot: no rows with usable gender + project.") }
  } else { log_msg("Women plot: 'gender' not in data.") }
  
  # 4) % Single bars (collapse divorced → single)
  if ("maritalstat" %in% names(df_ready)) {
    d_m <- df_ready %>% dplyr::filter(!is.na(.data[[proj_col]]), !is.na(maritalstat))
    if (nrow(d_m)) {
      d_m <- d_m %>%
        dplyr::mutate(mstat = as.character(maritalstat),
                      mstat = dplyr::if_else(mstat == "divorced", "single", mstat),
                      is_single = mstat == "single")
      single_pct <- d_m %>%
        dplyr::group_by(.data[[proj_col]]) %>%
        dplyr::summarise(N = dplyr::n(),
                         n_single = sum(is_single, na.rm = TRUE),
                         pct_single = ifelse(N > 0, 100 * n_single / N, NA_real_),
                         .groups = "drop") %>%
        dplyr::mutate(palette_key = to_palette_key(.data[[proj_col]]))
      pal <- vanilla_colors[unique(single_pct$palette_key)]; pal <- pal[!is.na(pal)]
      use_manual <- length(pal) > 0
      single_pct[[proj_col]] <- factor(single_pct[[proj_col]], levels = single_pct[[proj_col]])
      p_s <- ggplot2::ggplot(single_pct, ggplot2::aes(x = .data[[proj_col]], y = pct_single, fill = palette_key)) +
        ggplot2::geom_col(width = 0.75) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", pct_single)), vjust = -0.3, size = 3) +
        ggplot2::labs(title = paste0(sample, " — % Single (divorced → single) by project"),
                      x = "Project", y = "Percentage single") +
        { if (use_manual) ggplot2::scale_fill_manual(values = pal, drop = FALSE) else ggplot2::scale_fill_discrete() } +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "none")
      ggplot2::ggsave(fs::path(out_dir, "single_percentage_by_project.png"), p_s, width = 8, height = 5.2, dpi = 150)
      log_msg("Saved: ", fs::path(out_dir, "single_percentage_by_project.png"))
    } else { log_msg("Single plot: no usable rows (maritalstat + project).") }
  } else { log_msg("Single plot: 'maritalstat' not in data.") }
  
  # 5) Height density overlay (percentage; alpha = 0.2)
  if ("height" %in% names(df_ready)) {
    d_h <- df_ready %>% dplyr::filter(!is.na(height), !is.na(.data[[proj_col]]))
    if (nrow(d_h)) {
      pal_keys <- to_palette_key(d_h[[proj_col]])
      pal <- vanilla_colors[unique(pal_keys)]; pal <- pal[!is.na(pal)]
      use_manual <- length(pal) > 0
      p_h <- ggplot2::ggplot(d_h, ggplot2::aes(x = height, color = pal_keys, fill = pal_keys)) +
        ggplot2::geom_density(ggplot2::aes(y = after_stat(density * 100)), alpha = 0.2, adjust = 1) +
        ggplot2::labs(title = paste0(sample, " — Height distribution by project"), x = "Height (cm)", y = "Percentage") +
        { if (use_manual) ggplot2::scale_color_manual(values = pal, drop = FALSE) else ggplot2::scale_color_discrete() } +
        { if (use_manual) ggplot2::scale_fill_manual(values = pal, drop = FALSE) else ggplot2::scale_fill_discrete() } +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.title = ggplot2::element_blank())
      ggplot2::ggsave(fs::path(out_dir, "height_density_percentage.png"), p_h, width = 8, height = 5.2, dpi = 150)
      log_msg("Saved: ", fs::path(out_dir, "height_density_percentage.png"))
    } else { log_msg("Height density: no complete rows (height + project).") }
  } else { log_msg("Height density: 'height' not in data.") }
  
  # 6) Weight density overlay (percentage; alpha = 0.2)
  if ("weight" %in% names(df_ready)) {
    d_w <- df_ready %>% dplyr::filter(!is.na(weight), !is.na(.data[[proj_col]]))
    if (nrow(d_w)) {
      pal_keys <- to_palette_key(d_w[[proj_col]])
      pal <- vanilla_colors[unique(pal_keys)]; pal <- pal[!is.na(pal)]
      use_manual <- length(pal) > 0
      p_wt <- ggplot2::ggplot(d_w, ggplot2::aes(x = weight, color = pal_keys, fill = pal_keys)) +
        ggplot2::geom_density(ggplot2::aes(y = after_stat(density * 100)), alpha = 0.2, adjust = 1) +
        ggplot2::labs(title = paste0(sample, " — Weight distribution by project"), x = "Weight (kg)", y = "Percentage") +
        { if (use_manual) ggplot2::scale_color_manual(values = pal, drop = FALSE) else ggplot2::scale_color_discrete() } +
        { if (use_manual) ggplot2::scale_fill_manual(values = pal, drop = FALSE) else ggplot2::scale_fill_discrete() } +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.title = ggplot2::element_blank())
      ggplot2::ggsave(fs::path(out_dir, "weight_density_percentage.png"), p_wt, width = 8, height = 5.2, dpi = 150)
      log_msg("Saved: ", fs::path(out_dir, "weight_density_percentage.png"))
    } else { log_msg("Weight density: no complete rows (weight + project).") }
  } else { log_msg("Weight density: 'weight' not in data.") }
  
  invisible(TRUE)
}


# ---- One-sample driver ------------------------------------------

process_sample <- function(sample,
                           questionnaire_path = NULL,
                           iteminfo_path = NULL,
                           scoring_df = NULL,
                           drop_criteria_fun = remove_flagged_rows) {
  clear_console()
  log_msg("\n--- Processing sample: ", sample, " ---")
  
  if (is.null(questionnaire_path)) questionnaire_path <- latest_questionnaire_for_sample(sample)
  if (is.na(questionnaire_path)) {
    log_msg("No questionnaire file found for sample '", sample, "'. Skipping.")
    return(invisible(NULL))
  }
  if (is.null(iteminfo_path)) iteminfo_path <- latest_iteminfo_for_sample(sample)
  if (is.null(scoring_df)) scoring_df <- read_scoring(latest_scoring())
  
  q_raw <- read_questionnaire(questionnaire_path)
  ii_all <- read_item_info(iteminfo_path)
  if (is.null(ii_all)) {
    log_msg("Item Information missing; cannot proceed with mapping and reverse coding. Skipping sample.")
    return(invisible(NULL))
  }
  
  dropped   <- drop_criteria_fun(q_raw, sample)
  q_clean0  <- dropped$clean
  q_discard <- dropped$discarded
  
  parts   <- split_id_and_item_columns(q_clean0, ii_all, scoring_df)
  id_cols <- parts$id_cols
  item_cols <- parts$item_cols
  ii <- parts$item_info_valid
  
  check_header_match(q_clean0, ii)
  
  col_map <- tibble::tibble(
    orig      = names(q_clean0),
    item_norm = normalize_id(names(q_clean0))
  ) %>%
    dplyr::inner_join(ii %>% dplyr::select(item_norm, scale, reverse_coded), by = "item_norm") %>%
    dplyr::distinct(orig, .keep_all = TRUE)
  
  minmax <- scoring_df %>% dplyr::select(scale, min, max)
  col_meta <- col_map %>% dplyr::left_join(minmax, by = "scale") %>% dplyr::select(orig, reverse_coded, min, max)
  
  q_final <- reverse_code_wide(q_clean0, unique(col_map$orig), col_meta)
  q_final <- q_final %>% dplyr::select(dplyr::any_of(id_cols), dplyr::everything())
  q_final <- attach_groupings(q_final, sample)
  
  # Enriched master (used for simple plots & exports)
  q_ready <- enrich_demographics_and_health_fields(q_final, sample)
  
  # >>> SIMPLE PLOTS ONLY
  plot_simple_demographics(q_ready, sample, questionnaire_path)
  
  # Keys & exports
  keys <- build_keys(ii)
  save_keys(keys, sample)
  export_per_project(q_ready, q_discard, sample)
  
  log_msg("--- Done sample: ", sample, " ---\n")
  invisible(list(
    data_clean = q_ready,
    data_discarded = q_discard,
    keys = keys
  ))
}

# ---- Main --------------------------------------------------------------------

ALL_SAMPLES <- c("adolescents", "adults", "children_p6", "children_parents", "parents_p6")
SAMPLES_TO_PROCESS <- c("adults")

SCORING_PATH <- latest_scoring()
if (is.na(SCORING_PATH)) {
  stop(glue::glue(
    "No scoring file found in '{DIR_INFO}'. Expected something like 'YYYY-MM-DD_Scoring.xlsx'."
  ))
}
SCORING <- read_scoring(SCORING_PATH)

results <- purrr::map(SAMPLES_TO_PROCESS, ~ process_sample(.x, scoring_df = SCORING))
