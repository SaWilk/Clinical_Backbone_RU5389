#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FOR: Separate Backbone Data by Project  — Cleaning & Export Pipeline
# Authors: Saskia Wilken (saskia.wilken@uni-hamburg.de, saskia.a.wilken@gmail.com))
# First edited: 2025-11-07 (SW)
#
# Description:
# Reads questionnaire data, filters bad rows, reverse-codes, exports per project,
# produces demographics/health plots (bars + histograms), collapses multiple-
# choice job/ownpsychdiagn into single plots, writes sanity logs, and keeps keys.
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
  "ggplot2", "forcats"
))

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
  
  id_candidates <- c("vp", "vpid", "participant", "participant_id", "participantid", "vp_id", "vp-id")
  group_candidates <- c("group", "grp", "status", "diagnosis", "dx", "health_status")
  
  out <- purrr::map_dfr(keep, function(sh) {
    df <- suppressMessages(readxl::read_excel(filepath, sheet = sh)) %>%
      janitor::clean_names()
    
    id_col <- intersect(names(df), id_candidates) |> purrr::pluck(1, .default = NA_character_)
    if (is.na(id_col)) {
      log_msg("Sheet '", sh, "' has no recognizable ID column — skipped.")
      return(NULL)
    }
    grp_col <- intersect(names(df), group_candidates) |> purrr::pluck(1, .default = NA_character_)
    if (is.na(grp_col)) {
      log_msg("Sheet '", sh, "' has no recognizable group column — skipped.")
      return(NULL)
    }
    
    p_val <- sub(glue::glue("^(p[^_]+)_{sample}$"), "\\1", sh)
    
    df %>%
      dplyr::transmute(
        p = p_val,
        vp_join = normalize_vpid(.data[[id_col]]),
        group = standardize_group(.data[[grp_col]])
      ) %>%
      dplyr::filter(!is.na(vp_join) & !is.na(group))
  })
  
  if (nrow(out) == 0) {
    log_msg("Groupings for '", sample, "' contained no usable rows after cleaning.")
    return(NULL)
  }
  
  out <- dplyr::distinct(out, p, vp_join, .keep_all = TRUE)
  log_msg("Loaded ", nrow(out), " group mappings for sample '", sample, "'.")
  out
}

attach_groupings <- function(df, sample, id_guess = c("vp","vpid","participant","participant_id","participantid")) {
  if (!"p" %in% names(df)) {
    log_msg("No 'p' column in data — cannot attach groupings. Returning original df.")
    return(df)
  }
  grp <- read_groupings_for_sample(sample)
  if (is.null(grp)) return(df)
  
  id_in_df <- intersect(names(df), id_guess)
  if (!length(id_in_df)) {
    log_msg("No recognizable participant ID column in df — joining by project only (may be too coarse).")
    df2 <- df %>%
      dplyr::left_join(grp %>% dplyr::select(p, group) %>% dplyr::distinct(), by = "p")
    return(df2)
  }
  
  id_col <- id_in_df[1]
  df %>%
    dplyr::mutate(.vp_join = normalize_vpid(.data[[id_col]])) %>%
    dplyr::left_join(grp, by = c("p", ".vp_join" = "vp_join")) %>%
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
  
  leaf <- ii %>%
    dplyr::group_by(scale, higher_order_subscale, subscale) %>%
    dplyr::summarise(items = list(unique(item_norm)), .groups = "drop")
  
  ho_level <- leaf %>%
    dplyr::group_by(scale, higher_order_subscale) %>%
    dplyr::summarise(
      subscales = list(tibble::tibble(subscale = subscale, items = items)),
      .groups = "drop"
    )
  
  nested <- ho_level %>%
    dplyr::group_by(scale) %>%
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
  
  if (!"p" %in% names(df_clean)) {
    log_msg("Column 'p' not found — skipping per-project split.")
    return(invisible(NULL))
  }
  
  df_split <- split(df_clean, df_clean$p)
  purrr::iwalk(df_split, function(dd, proj) {
    safe_proj <- gsub("[^A-Za-z0-9_-]+", "_", proj)
    filepath <- fs::path(out_dir, glue::glue("{sample}_project-{safe_proj}_clean.csv"))
    readr::write_csv(dd, filepath)
  })
  
  log_msg(glue::glue("Exported {length(df_split)} project-level files for sample '{sample}'."))
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

label_demographics_health <- function(df) {
  out <- df
  
  out <- out %>%
    dplyr::mutate(
      dplyr::across(
        c(
          tidyselect::any_of("gender"),
          tidyselect::starts_with("parentsgender"),
          tidyselect::starts_with("siblingsgender"),
          tidyselect::starts_with("cildrengender"),
          tidyselect::starts_with("childrengender"),
          tidyselect::matches("^children(gender)?", ignore.case = TRUE)
        ),
        ~ factor(as.character(.x),
                 levels = c("1","2","3","0","-oth-"),
                 labels = c("female","male","nonbinary","no gender","other"))
      )
    )
  
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
                                      "diploma","bachelor","master","PhD","other")
        )
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
  
  if ("maritalstat" %in% names(out)) {
    out <- out %>%
      dplyr::mutate(
        maritalstat = factor(as.character(maritalstat),
                             levels = c("0","1","2","3","4"),
                             labels = c("single","relationship","married","divorced","widowed")
        )
      )
  }
  
  out <- out %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of(c("eyesight","hearing")),
        ~ factor(as.character(.x),
                 levels = c("0","1","2"),
                 labels = c("normal","corrected","not corrected"))
      )
    )
  
  out <- out %>%
    dplyr::mutate(
      dplyr::across(
        c(
          tidyselect::any_of(c("psychomedication","othermedication","ownpsychdisorder")),
          tidyselect::contains("contact")
        ),
        ~ factor(as.character(.x), levels = c("0","1"), labels = c("no","yes"))
      )
    )
  out
}

# -------- Multiple-choice helpers (robust to different naming styles) ---------

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

# Main plotting for demographics & health -------------------------------------

plot_demographics_and_health <- function(df, ii_all, sample, questionnaire_path) {
  # Reset sanity log
  if (fs::file_exists(SANITY_LOG_FILE)) fs::file_delete(SANITY_LOG_FILE)
  fs::file_create(SANITY_LOG_FILE) |> suppressWarnings()
  
  ii_dh <- ii_all %>%
    dplyr::mutate(item_norm = normalize_id(.data$item),
                  scale = as.character(.data$scale)) %>%
    dplyr::filter(tolower(scale) %in% c("demographics","health"))
  if (nrow(ii_dh) == 0) {
    log_msg("No Item Information rows for scales 'demographics'/'health' — skipping plots.")
    return(invisible(NULL))
  }
  col_map <- tibble::tibble(orig = names(df), item_norm = normalize_id(names(df)))
  cols <- col_map %>%
    dplyr::inner_join(ii_dh %>% dplyr::select(item_norm), by = "item_norm") %>%
    dplyr::pull(orig) %>% unique()
  if (!length(cols)) {
    log_msg("No matching demographics/health columns found in data — skipping plots.")
    return(invisible(NULL))
  }
  date_str <- extract_date_string(questionnaire_path)
  out_dir  <- fs::path(DIR_ANALYSIS_BASE, paste0(date_str, "_", sample))
  if (fs::dir_exists(out_dir)) {
    old_files <- fs::dir_ls(out_dir, type = "file", glob = "*")
    if (length(old_files)) fs::file_delete(old_files)
  } else {
    fs::dir_create(out_dir)
  }
  sub <- df %>%
    dplyr::select(dplyr::any_of(c("submitdate","vpid","vp","participantid","p","age","height","weight", cols)))
  
  sub <- label_demographics_health(sub)
  
  if ("age" %in% names(sub)) sub <- sub %>% dplyr::rename(date_birth = age)
  sub <- sub %>%
    dplyr::mutate(
      quest_date = suppressWarnings(as.Date(submitdate)),
      date_birth = suppressWarnings(as.Date(date_birth)),
      age_years_raw = dplyr::if_else(
        !is.na(date_birth) & !is.na(quest_date),
        as.integer(floor(lubridate::interval(date_birth, quest_date) / lubridate::years(1))),
        as.integer(NA)
      )
    )
  
  get_id_vec <- function(d) {
    if ("vpid" %in% names(d)) d$vpid
    else if ("vp" %in% names(d)) d$vp
    else if ("participantid" %in% names(d)) d$participantid
    else rep(NA_character_, nrow(d))
  }
  
  if (tolower(sample) %in% c("adults","adolescents")) {
    height_num <- suppressWarnings(as.numeric(sub$height))
    weight_num <- suppressWarnings(as.numeric(sub$weight))
    age_bad    <- !is.na(sub$age_years_raw) & sub$age_years_raw < 5L
    height_bad <- !is.na(height_num) & height_num > 250
    weight_bad <- !is.na(weight_num) & weight_num < 35
    rep_rows <- function(idx, var, values, rule) {
      if (any(idx, na.rm = TRUE)) {
        tibble::tibble(
          vpid = get_id_vec(sub)[idx],
          p = if ("p" %in% names(sub)) sub$p[idx] else NA_character_,
          value = values[idx]
        ) %>% log_sanity(sample, ., var, rule)
      }
    }
    rep_rows(age_bad,    "age",    sub$age_years_raw, "below sensible threshold 5 years")
    rep_rows(height_bad, "height", height_num,        "above sensible threshold 250 cm")
    rep_rows(weight_bad, "weight", weight_num,        "below sensible threshold 35 kg")
    sub <- sub %>%
      dplyr::mutate(
        age_years = dplyr::if_else(age_bad, as.integer(NA), age_years_raw),
        height    = dplyr::if_else(height_bad, NA_real_, height_num),
        weight    = dplyr::if_else(weight_bad, NA_real_, weight_num)
      )
  } else {
    sub <- sub %>%
      dplyr::mutate(
        age_years = age_years_raw,
        height = suppressWarnings(as.numeric(height)),
        weight = suppressWarnings(as.numeric(weight))
      )
  }
  
  save_plot <- function(p, fname) {
    f <- fs::path(out_dir, fname)
    ggplot2::ggsave(filename = f, plot = p, width = 7, height = 5, dpi = 150, limitsize = FALSE)
    log_msg("Saved: ", f)
  }
  N_total <- nrow(sub)
  
  # Histograms
  hist_vars <- intersect(c("age_years","height","weight"), names(sub))
  for (v in hist_vars) {
    vec <- sub[[v]]
    n_non_na <- sum(!is.na(vec))
    mean_val <- suppressWarnings(mean(vec, na.rm = TRUE))
    median_val <- suppressWarnings(stats::median(vec, na.rm = TRUE))
    df_v <- tibble::tibble(value = vec)
    p <- ggplot2::ggplot(df_v, ggplot2::aes(x = value)) +
      ggplot2::geom_histogram(bins = 30, na.rm = TRUE) +
      ggplot2::labs(title = paste0(sample, " — Histogram of ", v), x = v, y = "Count") +
      ggplot2::geom_vline(xintercept = median_val, linetype = "solid") +
      ggplot2::geom_vline(xintercept = mean_val,   linetype = "dashed") +
      ggplot2::annotate("text", x = Inf, y = Inf,
                        label = paste0("N=", n_non_na, "  mean=", sprintf("%.2f", mean_val),
                                       "  median=", sprintf("%.2f", median_val)),
                        hjust = 1.05, vjust = 1.5, size = 3)
    save_plot(p, paste0("hist_", safe_var(v), ".png"))
  }
  
  # Categorical bars (exclude job/ownpsychdiagn variants + gender_other etc.)
  vars_for_bars <- setdiff(cols, c("age","height","weight","submitdate"))
  # Exclude job bracket & underscore forms
  ex_job <- grepl("^job\\[[^\\]]+\\]$", vars_for_bars, ignore.case = TRUE) |
    grepl("^job_\\d+_?$",        vars_for_bars, ignore.case = TRUE) |
    grepl("^job_(other)$",       vars_for_bars, ignore.case = TRUE) |
    grepl("^job\\[other\\]$",    vars_for_bars, ignore.case = TRUE)
  # Exclude ownpsychdiagn bracket & underscore forms
  ex_diag <- grepl("^ownpsychdiagn(osis)?\\[[^\\]]+\\]$", vars_for_bars, ignore.case = TRUE) |
    grepl("^ownpsychdiagn(osis)?_[A-Za-z]+_?$",  vars_for_bars, ignore.case = TRUE) |
    grepl("^ownpsychdiagn(osis)?_(other)$",      vars_for_bars, ignore.case = TRUE) |
    grepl("^ownpsychdiagn(osis)?\\[other\\]$",   vars_for_bars, ignore.case = TRUE)
  # Exclude gender_other (and variants), [other] free-text, and explicit otherdrug/psychodrug
  ex_misc <- grepl("gender[_]?other", vars_for_bars, ignore.case = TRUE) |
    grepl("\\[other\\]$|_other$", vars_for_bars, ignore.case = TRUE) |
    grepl("otherdrug|psychodrug", vars_for_bars, ignore.case = TRUE)
  
  vars_for_bars <- vars_for_bars[!(ex_job | ex_diag | ex_misc)]
  
  # Belt & suspenders: remove anything starting with job/ownpsychdiagn anyway
  vars_for_bars <- vars_for_bars[!grepl("^job($|\\[|_)", vars_for_bars, ignore.case = TRUE)]
  vars_for_bars <- vars_for_bars[!grepl("^ownpsychdiagn(osis)?($|\\[|_)", vars_for_bars, ignore.case = TRUE)]
  
  if (length(vars_for_bars)) {
    for (v in vars_for_bars) {
      x <- sub[[v]]
      xf <- if (is.factor(x)) x else factor(as.character(x))
      xf <- forcats::fct_explicit_na(xf, na_level = "missing")
      df_v <- tibble::tibble(value = xf)
      n_non_na <- sum(!is.na(x))
      p <- ggplot2::ggplot(df_v, ggplot2::aes(x = value)) +
        ggplot2::geom_bar(na.rm = TRUE) +
        ggplot2::geom_text(
          ggplot2::aes(label = paste0(sprintf("%.1f", 100*after_stat(count)/N_total), "%")),
          stat = "count", vjust = -0.2, size = 3
        ) +
        ggplot2::labs(title = paste0(sample, " — ", v), x = v, y = "Count") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)) +
        ggplot2::annotate("text", x = Inf, y = Inf,
                          label = paste0("N=", n_non_na),
                          hjust = 1.1, vjust = 1.5, size = 3)
      save_plot(p, paste0("bar_", safe_var(v), ".png"))
    }
  } else {
    log_msg("No variables to bar-plot after exclusions.")
  }
  
  # Multiple-choice aggregates (single plots)
  job_count <- compute_job_count(sub)
  plot_mc_counts(
    job_count,
    title   = paste0(sample, " — Current occupation (multiple responses)"),
    out_file = fs::path(out_dir, "bar_job_multiple_choice.png"),
    N_total = N_total
  )
  
  diag_count <- compute_diag_count(sub)
  plot_mc_counts(
    diag_count,
    title    = paste0(sample, " — Own psychological diagnosis (multiple responses)"),
    out_file = fs::path(out_dir, "bar_ownpsychdiagnosis_multiple_choice.png"),
    N_total  = N_total
  )
  
  invisible(TRUE)
}

# ---- One-sample driver ------------------------------------------

process_sample <- function(sample,
                           questionnaire_path = NULL,
                           iteminfo_path = NULL,
                           scoring_df = NULL,
                           drop_criteria_fun = remove_flagged_rows) {
  
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
    dplyr::inner_join(ii %>% dplyr::select(item_norm, scale, reverse_coded),
                      by = "item_norm") %>%
    dplyr::distinct(orig, .keep_all = TRUE)
  
  minmax <- scoring_df %>% dplyr::select(scale, min, max)
  col_meta <- col_map %>%
    dplyr::left_join(minmax, by = "scale") %>%
    dplyr::select(orig, reverse_coded, min, max)
  
  q_final <- reverse_code_wide(q_clean0, unique(col_map$orig), col_meta)
  q_final <- q_final %>% dplyr::select(dplyr::any_of(id_cols), dplyr::everything())
  q_final <- attach_groupings(q_final, sample)
  
  plot_demographics_and_health(q_final, ii_all, sample, questionnaire_path)
  
  keys <- build_keys(ii)
  save_keys(keys, sample)
  
  export_per_project(q_final, q_discard, sample)
  
  log_msg("--- Done sample: ", sample, " ---\n")
  invisible(list(
    data_clean = q_final,
    data_discarded = q_discard,
    keys = keys
  ))
}

# ---- Main --------------------------------------------------------------------

ALL_SAMPLES <- c("adolescents", "adults", "children_p6", "children_parents", "parents_p6")
SAMPLES_TO_PROCESS <- c("adults")

SCORING <- read_scoring(latest_scoring())

results <- purrr::map(SAMPLES_TO_PROCESS, ~ process_sample(.x, scoring_df = SCORING))

log_msg("\nPipeline finished.\n")
