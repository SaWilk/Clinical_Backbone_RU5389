# RU5389 cognitive scoring: exploratory quality assessment (BACS, WCST, LNS)
# Place beside prep_03_Cognitive_Scoring_pipeline.R and run AFTER that script.
# Reads every project-specific and ALL cognitive_scores workbook independently.
# It does not change scores, exclude participants, or rewrite pipeline outputs.

required_packages <- c("readxl", "writexl", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install missing R packages: ", paste(missing_packages, collapse = ", "))
}

script_directory <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  script_arg <- grep("^--file=", args, value = TRUE)
  if (length(script_arg)) return(dirname(normalizePath(sub("^--file=", "", script_arg[1]), mustWork = FALSE)))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    active <- rstudioapi::getSourceEditorContext()$path
    if (length(active) && !is.na(active) && nzchar(active)) return(dirname(normalizePath(active, mustWork = FALSE)))
  }
  normalizePath(getwd(), mustWork = FALSE)
}

script_dir <- script_directory()
input_root <- file.path(script_dir, "01_project_data")
output_root <- Sys.getenv(
  "COGNITIVE_QC_OUTPUT_DIR",
  unset = "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389/out/cognitive_tests"
)
rt_fast_cutoff_ms <- 400
fast_trial_proportion_flag <- 0.20
mean_rt_slow_cutoff_ms <- 4000
stratification_path <- Sys.getenv(
  "COGNITIVE_STRATIFICATION_PATH",
  unset = "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389/03_analysis_input/adults_adolescents_stratification_info.xlsx"
)
presentation_scores <- c(
  "score_BACS_correct" = "BACS correct",
  "score_WCST_pers_err" = "WCST perseverative errors",
  "score_WCST_correct" = "WCST correct",
  "score_LNS_correct" = "LNS correct"
)

if (!dir.exists(input_root)) stop("Input folder does not exist: ", input_root)
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

normalize_path <- function(path) normalizePath(path, winslash = "/", mustWork = FALSE)

extract_date_from_name <- function(x) {
  match <- regmatches(x, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}", x))
  date <- suppressWarnings(as.Date(match))
  ifelse(is.na(date), 0, as.numeric(date))
}

find_file_case_insensitive <- function(directory, filename) {
  if (is.na(directory) || is.na(filename) || !dir.exists(directory)) return(NA_character_)
  direct <- file.path(directory, basename(filename))
  if (file.exists(direct)) return(direct)
  entries <- list.files(directory, full.names = TRUE, recursive = FALSE)
  hit <- entries[tolower(basename(entries)) == tolower(basename(filename))]
  if (length(hit)) hit[1] else NA_character_
}

# Use the same raw-file resolution as the scoring pipeline, without adding
# any provenance column or changing the scoring workbooks.
candidate_data_directories <- function(master) {
  fallback <- c(
    file.path(input_root, "all_projects_backbone", "raw_data", "experiment_data"),
    file.path(input_root, "all_projects_backbone", "experiment_data")
  )
  search_roots <- unique(c(dirname(master$path), fallback[file.exists(fallback)]))
  directories <- unique(unlist(lapply(search_roots, function(x) {
    list.dirs(x, recursive = TRUE, full.names = TRUE)
  }), use.names = FALSE))
  directories <- directories[grepl("_cogtest_data", basename(directories), ignore.case = TRUE)]
  directories <- directories[
    grepl(tolower(paste0("_", master$sample, "_cogtest_data")),
          tolower(basename(directories)), fixed = TRUE)
  ]
  has_exp <- grepl("exp[-_/]?[12]", normalize_path(directories), ignore.case = TRUE)
  if (is.na(master$exp_label)) {
    directories <- directories[!has_exp]
  } else {
    exp_number <- sub("exp-", "", master$exp_label, fixed = TRUE)
    directories <- directories[
      grepl(paste0("exp[-_/]?", exp_number, "(?:/|$)"),
            normalize_path(directories), ignore.case = TRUE, perl = TRUE)
    ]
  }
  is_pilot <- grepl("pilot", normalize_path(directories), ignore.case = TRUE)
  directories <- if (master$pilot) directories[is_pilot] else directories[!is_pilot]
  if (!length(directories)) return(character())
  info <- file.info(directories)
  order_key <- order(extract_date_from_name(basename(directories)),
                     as.numeric(info$mtime), decreasing = TRUE, na.last = TRUE)
  directories[order_key]
}

resolve_trial_path <- function(filename, directories) {
  if (length(filename) != 1L || is.na(filename) || !nzchar(trimws(filename))) return(NA_character_)
  if (file.exists(filename)) return(normalize_path(filename))
  for (directory in directories) {
    hit <- find_file_case_insensitive(directory, filename)
    if (!is.na(hit)) return(hit)
  }
  NA_character_
}

get_run_info <- function(path) {
  info <- tryCatch(as.data.frame(readxl::read_excel(path, sheet = "run_info")),
                   error = function(e) NULL)
  if (is.null(info) || !all(c("field", "value") %in% names(info))) return(NULL)
  setNames(as.character(info$value), as.character(info$field))
}

empty_rt <- function(status) {
  data.frame(
    qc_BACS_raw_status = status,
    qc_BACS_experiment_trials = NA_integer_,
    qc_BACS_trials_under_400ms = NA_integer_,
    qc_BACS_prop_trials_under_400ms = NA_real_,
    stringsAsFactors = FALSE
  )
}

read_bacs_rt <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(list(summary = empty_rt("Rohdatei fehlt"), trials = numeric()))
  }
  raw <- tryCatch(
    utils::read.table(
      path, header = FALSE, fill = TRUE, stringsAsFactors = FALSE,
      quote = "", comment.char = "", na.strings = c("NA", "NaN", "null")
    ),
    error = function(e) e
  )
  if (inherits(raw, "error") || !is.data.frame(raw) || ncol(raw) < 8L || !nrow(raw)) {
    return(list(summary = empty_rt("Rohdatei leer oder unlesbar"), trials = numeric()))
  }
  training <- tolower(trimws(as.character(raw[[3]]))) == "training"
  training[is.na(training)] <- FALSE
  rt <- suppressWarnings(as.numeric(as.character(raw[[8]][!training])))
  n_trials <- length(rt)
  n_fast <- sum(is.finite(rt) & rt < rt_fast_cutoff_ms)
  list(
    summary = data.frame(
      qc_BACS_raw_status = if (n_trials) "Gelesen" else "Keine Experiment-Trials",
      qc_BACS_experiment_trials = n_trials,
      qc_BACS_trials_under_400ms = n_fast,
      qc_BACS_prop_trials_under_400ms = if (n_trials) n_fast / n_trials else NA_real_
    ),
    trials = rt[is.finite(rt)]
  )
}

one_distribution <- function(x, variable) {
  n_total <- length(x)
  x <- suppressWarnings(as.numeric(as.character(x)))
  x <- x[is.finite(x)]
  n <- length(x)
  m <- if (n) mean(x) else NA_real_
  sd_x <- if (n >= 2L) stats::sd(x) else NA_real_
  skew <- kurt <- NA_real_
  if (n >= 3L && is.finite(sd_x) && sd_x > 0) {
    z <- (x - m) / sqrt(mean((x - m)^2))
    skew <- mean(z^3)
    kurt <- mean(z^4) # Pearson kurtosis: normal distribution = 3
  }
  shapiro <- if (n >= 3L && n <= 5000L && length(unique(x)) >= 3L) {
    tryCatch(stats::shapiro.test(x), error = function(e) NULL)
  } else NULL
  data.frame(
    variable = variable, n_valid = n, n_missing = n_total - n,
    mean = m, sd = sd_x,
    median = if (n) stats::median(x) else NA_real_,
    q1 = if (n) as.numeric(stats::quantile(x, .25)) else NA_real_,
    q3 = if (n) as.numeric(stats::quantile(x, .75)) else NA_real_,
    iqr = if (n) stats::IQR(x) else NA_real_,
    min = if (n) min(x) else NA_real_, max = if (n) max(x) else NA_real_,
    skewness = skew, kurtosis = kurt,
    shapiro_W = if (is.null(shapiro)) NA_real_ else unname(shapiro$statistic),
    shapiro_p = if (is.null(shapiro)) NA_real_ else shapiro$p.value
  )
}

one_correlation <- function(x, y, var1, var2, method) {
  x <- suppressWarnings(as.numeric(as.character(x)))
  y <- suppressWarnings(as.numeric(as.character(y)))
  valid <- is.finite(x) & is.finite(y)
  x <- x[valid]; y <- y[valid]
  result <- if (length(x) >= 3L && length(unique(x)) > 1L && length(unique(y)) > 1L) {
    tryCatch(suppressWarnings(stats::cor.test(x, y, method = method, exact = FALSE)),
             error = function(e) NULL)
  } else NULL
  data.frame(
    variable_1 = var1, variable_2 = var2, method = method, n_complete = length(x),
    estimate = if (is.null(result)) NA_real_ else unname(result$estimate),
    p_value = if (is.null(result)) NA_real_ else result$p.value
  )
}

presentation_theme <- function() {
  ggplot2::theme_minimal(base_size = 22) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 28, face = "bold", margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(size = 20, margin = ggplot2::margin(b = 16)),
      axis.title = ggplot2::element_text(size = 23, face = "bold"),
      axis.text = ggplot2::element_text(size = 19, colour = "#222222"),
      legend.title = ggplot2::element_text(size = 20, face = "bold"),
      legend.text = ggplot2::element_text(size = 19),
      panel.grid.major = ggplot2::element_line(linewidth = .65, colour = "#d7dfe6"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(linewidth = 1.1, colour = "#333333"),
      axis.ticks = ggplot2::element_line(linewidth = 1.1)
    )
}

nice_label <- function(variable) {
  if (variable %in% names(presentation_scores)) unname(presentation_scores[[variable]]) else variable
}

distribution_plot <- function(values, label, n_participants = length(values)) {
  values <- suppressWarnings(as.numeric(as.character(values)))
  values <- values[is.finite(values)]
  if (!length(values)) return(NULL)
  m <- mean(values)
  s <- if (length(values) >= 2L) stats::sd(values) else NA_real_
  frame <- data.frame(value = values)
  p <- ggplot2::ggplot(frame, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_histogram(bins = min(30L, max(1L, length(unique(values)))),
                            fill = "#2978a0", colour = "white", linewidth = .7) +
    ggplot2::geom_vline(xintercept = m, colour = "#bf3f39", linewidth = 1.7) +
    ggplot2::labs(
      title = sprintf("%s (N = %d participants)", label, n_participants),
      subtitle = if (is.finite(s)) sprintf("Mean %.2f (red); mean ± 1 SD (dashed, SD = %.2f)", m, s)
                 else sprintf("Mean %.2f (red); SD unavailable", m),
      x = label, y = "Participants"
    ) + presentation_theme()
  if (is.finite(s)) p <- p +
    ggplot2::geom_vline(xintercept = c(m - s, m + s), colour = "#bf3f39",
                        linetype = "longdash", linewidth = 1.35)
  p
}

save_slide_plot <- function(plot, path, width = 13.333, height = 7.5) {
  ggplot2::ggsave(path, plot = plot, width = width, height = height,
                  units = "in", dpi = 300, bg = "white", limitsize = FALSE)
}

write_plot_pdf <- function(scores, score_vars, trial_rts, pdf_path, slide_dir = NULL) {
  grDevices::pdf(pdf_path, width = 13.333, height = 7.5, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  n_pages <- 0L
  for (variable in score_vars) {
    values <- suppressWarnings(as.numeric(as.character(scores[[variable]])))
    p <- distribution_plot(values, nice_label(variable), sum(is.finite(values)))
    if (is.null(p)) next
    print(p); n_pages <- n_pages + 1L
    if (!is.null(slide_dir) && variable %in% names(presentation_scores)) {
      save_slide_plot(p, file.path(slide_dir, paste0(variable, "_distribution.png")))
    }
  }
  if (length(trial_rts)) {
    p <- distribution_plot(trial_rts, "BACS experiment-trial RT (ms)",
                           n_participants = sum(!is.na(scores$score_BACS_mean_rt)))
    print(p); n_pages <- n_pages + 1L
  }
  if (!n_pages) {
    graphics::plot.new()
    graphics::text(.5, .5, "No finite scores or BACS reaction times available", cex = 1.7)
  }
}

normalize_id <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  digits <- !is.na(x) & grepl("^[0-9]+$", x)
  x[digits] <- sub("^0+(?=[0-9])", "", x[digits], perl = TRUE)
  x
}

normalize_sample <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x %in% c("adult", "adults")] <- "adults"
  x[x %in% c("adolescent", "adolescents")] <- "adolescents"
  x
}

normalize_project <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("^(project|projekt|p)[ _-]*", "", x)
  x <- sub("_backbone$", "", x)
  x[x %in% c("", "na", "nan")] <- NA_character_
  x
}

diagnosis_group <- function(x) {
  x <- tolower(trimws(as.character(x)))
  result <- rep("unknown", length(x))
  hc <- !is.na(x) & grepl("^(hc|healthy[ _-]*control|control|gesund|kontroll)", x)
  diagnosed <- !is.na(x) & grepl(
    "patient|diagnos|clinical|klinisch|schiz|psychos|psychot|ocd|anxiety|angst|depress|bipolar",
    x
  )
  result[hc] <- "HC"
  result[diagnosed & !hc] <- "with_diagnosis"
  factor(result, levels = c("HC", "with_diagnosis", "unknown"))
}

gender_group <- function(x) {
  x <- tolower(trimws(as.character(x)))
  result <- rep(NA_character_, length(x))
  result[x %in% c("m", "male", "man", "men", "männlich", "mann")] <- "Men"
  result[x %in% c("f", "female", "woman", "women", "weiblich", "frau")] <- "Women"
  factor(result, levels = c("Men", "Women"))
}

read_stratification_file <- function(path) {
  wanted <- c("sample", "vp_id", "project", "age", "gender", "group")
  ext <- tolower(tools::file_ext(path))
  candidates <- tryCatch({
    if (ext %in% c("xlsx", "xls")) {
      lapply(readxl::excel_sheets(path), function(sheet) readxl::read_excel(path, sheet = sheet))
    } else if (ext == "rds") {
      list(readRDS(path))
    } else if (ext == "tsv") {
      list(utils::read.delim(path, check.names = FALSE))
    } else if (ext == "csv") {
      list(utils::read.csv(path, check.names = FALSE),
           utils::read.csv2(path, check.names = FALSE))
    } else list()
  }, error = function(e) { warning("Cannot read stratification file ", path, ": ", conditionMessage(e)); list() })
  for (candidate in candidates) {
    if (!is.data.frame(candidate)) next
    names(candidate) <- tolower(trimws(names(candidate)))
    if (!all(wanted %in% names(candidate))) next
    result <- as.data.frame(candidate[wanted], stringsAsFactors = FALSE)
    result$source_file <- path
    return(result)
  }
  warning("No sheet/table with sample, vp_id, project, age, gender, group: ", path)
  NULL
}

load_stratification <- function(path) {
  files <- if (dir.exists(path)) {
    list.files(path, pattern = "\\.(xlsx|xls|csv|tsv|rds)$", recursive = TRUE,
               full.names = TRUE, ignore.case = TRUE)
  } else if (file.exists(path)) path else character()
  files <- files[!startsWith(basename(files), "~$")]
  if (!length(files)) {
    warning("Stratification info not found: ", path)
    return(NULL)
  }
  files <- files[order(file.info(files)$mtime, decreasing = TRUE)]
  parts <- Filter(Negate(is.null), lapply(files, read_stratification_file))
  if (!length(parts)) return(NULL)
  info <- do.call(rbind, parts)
  info$sample <- normalize_sample(info$sample)
  info$vp_id <- normalize_id(info$vp_id)
  info$project <- normalize_project(info$project)
  key <- paste(info$sample, info$vp_id, info$project, sep = "|")
  repeats <- duplicated(key)
  if (any(repeats)) {
    warning(sum(repeats), " duplicate stratification row(s); newest source file/first row retained")
    info <- info[!repeats, , drop = FALSE]
  }
  rownames(info) <- NULL
  info
}

join_stratification <- function(scores, info) {
  scores$sample <- normalize_sample(scores$sample)
  scores$vp_id <- normalize_id(scores$score_vp_id)
  scores$project <- normalize_project(scores$project)
  if (is.null(info)) {
    scores$group_raw <- NA_character_
    scores$gender_raw <- NA_character_
    scores$age <- NA_real_
    scores$stratification_match <- "not_available"
  } else {
    exact <- match(paste(scores$sample, scores$vp_id, scores$project, sep = "|"),
                   paste(info$sample, info$vp_id, info$project, sep = "|"))
    # A project-free master can only be joined on sample and ID if that ID
    # occurs exactly once across stratification projects.
    missing <- which(is.na(exact) & is.na(scores$project))
    id_key <- paste(info$sample, info$vp_id, sep = "|")
    unique_id_key <- id_key[!duplicated(id_key) & !duplicated(id_key, fromLast = TRUE)]
    fallback <- match(paste(scores$sample[missing], scores$vp_id[missing], sep = "|"), id_key)
    fallback[!paste(scores$sample[missing], scores$vp_id[missing], sep = "|") %in% unique_id_key] <- NA_integer_
    exact[missing] <- fallback
    scores$stratification_match <- ifelse(is.na(exact), "unmatched", "exact")
    scores$stratification_match[missing[!is.na(fallback)]] <- "unique_sample_id"
    scores$group_raw <- as.character(info$group[exact])
    scores$gender_raw <- as.character(info$gender[exact])
    scores$age <- suppressWarnings(as.numeric(gsub(",", ".", as.character(info$age[exact]), fixed = TRUE)))
    scores$project[is.na(scores$project)] <- info$project[exact[is.na(scores$project)]]
  }
  scores$group_status <- diagnosis_group(scores$group_raw)
  scores$qc_project_3_4_5_unknown_to_HC <- !is.na(scores$project) &
    scores$project %in% c("3", "4", "5") & scores$group_status == "unknown"
  scores$group_status[scores$qc_project_3_4_5_unknown_to_HC] <- "HC"
  scores$qc_project_6_hc_override <- !is.na(scores$project) & scores$project == "6"
  scores$group_status[scores$qc_project_6_hc_override] <- "HC"
  scores$gender_status <- gender_group(scores$gender_raw)
  scores
}

presentation_colours <- c("HC" = "#1B7999", "with_diagnosis" = "#CB5E5A", "unknown" = "#9D9D9D")
jitter_score_points <- function(variable) {
  identical(variable, "score_LNS_correct") || grepl("^score_WCST_", variable)
}

comparison_theme <- function() {
  ggplot2::theme_minimal(base_size = 21) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#D8E0E6", linewidth = .9),
      axis.line = ggplot2::element_line(colour = "#30363B", linewidth = 1.1),
      axis.ticks = ggplot2::element_line(colour = "#30363B", linewidth = 1.1),
      axis.ticks.length = grid::unit(.15, "in"),
      axis.text = ggplot2::element_text(size = 20, colour = "#222222", face = "bold"),
      axis.title = ggplot2::element_text(size = 23, colour = "#111111", face = "bold"),
      plot.title = ggplot2::element_text(size = 23, face = "bold", lineheight = 1.17,
                                       margin = ggplot2::margin(b = 20)),
      plot.subtitle = ggplot2::element_text(size = 19, margin = ggplot2::margin(b = 12)),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 21, face = "bold"),
      legend.text = ggplot2::element_text(size = 20),
      legend.key.size = grid::unit(.32, "in"),
      plot.margin = ggplot2::margin(25, 35, 25, 30)
    )
}

comparison_title <- function(frame, variable, groups = c("HC", "with_diagnosis", "unknown")) {
  counts <- vapply(groups, function(group) sum(as.character(frame$group_status) == group), integer(1))
  means <- vapply(groups, function(group) {
    values <- frame$value[as.character(frame$group_status) == group]
    if (!length(values)) "NA" else sprintf("%.2f", mean(values))
  }, character(1))
  paste0(
    nice_label(variable), " | N = ", nrow(frame), " (",
    paste(paste0(groups, " ", counts), collapse = ", "), ")\n",
    "Mean score: ", paste(paste0(groups, " ", means), collapse = "; ")
  )
}

project_plot <- function(data, variable) {
  frame <- data.frame(project = ifelse(is.na(data$project), "unknown", data$project),
                      value = suppressWarnings(as.numeric(data[[variable]])),
                      group_status = data$group_status)
  frame <- frame[is.finite(frame$value), , drop = FALSE]
  if (!nrow(frame)) return(NULL)
  order_levels <- unique(frame$project[order(suppressWarnings(as.numeric(frame$project)), frame$project, na.last = TRUE)])
  frame$project <- factor(frame$project, levels = order_levels,
                          labels = ifelse(order_levels == "unknown", "unknown", paste0("P", order_levels)))
  jitter_height <- if (jitter_score_points(variable)) .12 else 0
  ggplot2::ggplot(frame, ggplot2::aes(
    x = .data$project, y = .data$value, colour = .data$group_status,
    fill = .data$group_status, group = interaction(.data$project, .data$group_status)
  )) +
    ggplot2::geom_boxplot(width = .68, linewidth = 1.25, outlier.shape = NA,
                          alpha = .16, position = ggplot2::position_dodge(width = .80)) +
    ggplot2::geom_point(alpha = .62, size = 2.7, stroke = 0,
                        position = ggplot2::position_jitterdodge(
                          jitter.width = .13, jitter.height = jitter_height,
                          dodge.width = .80, seed = 5389
                        )) +
    ggplot2::scale_colour_manual(values = presentation_colours, drop = FALSE) +
    ggplot2::scale_fill_manual(values = presentation_colours, drop = FALSE) +
    ggplot2::scale_x_discrete(drop = TRUE) +
    ggplot2::labs(title = comparison_title(frame, variable),
                  x = "Project", y = nice_label(variable), colour = "Group", fill = "Group") +
    comparison_theme()
}

gender_plot <- function(data, variable, test_result) {
  frame <- data.frame(gender = data$gender_status,
                      value = suppressWarnings(as.numeric(data[[variable]])),
                      group_status = data$group_status)
  frame <- frame[!is.na(frame$gender) & is.finite(frame$value), , drop = FALSE]
  if (!nrow(frame)) return(NULL)
  score_jitter <- jitter_score_points(variable)
  jitter_height <- if (score_jitter) .12 else 0
  format_p <- function(p) if (p < .001) "< .001" else sprintf("= %.3f", p)
  test_label <- if (is.finite(test_result$p_value)) {
    sprintf("Welch t(%.1f) = %.2f; p %s; Holm-adjusted p %s",
            test_result$df, test_result$t_statistic,
            format_p(test_result$p_value), format_p(test_result$p_holm))
  } else "Welch test unavailable (fewer than two scores in a gender group)"
  ggplot2::ggplot(frame, ggplot2::aes(
    x = .data$gender, y = .data$value, colour = .data$group_status,
    fill = .data$group_status, group = interaction(.data$gender, .data$group_status)
  )) +
    ggplot2::geom_boxplot(width = .68, linewidth = 1.25, outlier.shape = NA,
                          alpha = .16, position = ggplot2::position_dodge(width = .80)) +
    ggplot2::geom_point(alpha = .62, size = 2.7, stroke = 0,
                        position = ggplot2::position_jitterdodge(
                          jitter.width = .13, jitter.height = jitter_height,
                          dodge.width = .80, seed = 5389
                        )) +
    ggplot2::scale_colour_manual(values = presentation_colours, drop = FALSE) +
    ggplot2::scale_fill_manual(values = presentation_colours, drop = FALSE) +
    ggplot2::labs(title = comparison_title(frame, variable),
                  subtitle = paste0(test_label, "; other/unknown gender excluded"),
                  x = "Gender", y = nice_label(variable), colour = "Group", fill = "Group") +
    comparison_theme()
}

gender_test_result <- function(data, variable) {
  values <- suppressWarnings(as.numeric(data[[variable]]))
  men <- values[!is.na(data$gender_status) & data$gender_status == "Men"]
  women <- values[!is.na(data$gender_status) & data$gender_status == "Women"]
  men <- men[is.finite(men)]; women <- women[is.finite(women)]
  test <- if (length(men) >= 2L && length(women) >= 2L) {
    tryCatch(stats::t.test(men, women), error = function(e) NULL)
  } else NULL
  data.frame(
    score = variable, method = "Welch t-test", n_men = length(men), n_women = length(women),
    mean_difference_men_minus_women = if (length(men) && length(women)) mean(men) - mean(women) else NA_real_,
    t_statistic = if (is.null(test)) NA_real_ else unname(test$statistic),
    df = if (is.null(test)) NA_real_ else unname(test$parameter),
    p_value = if (is.null(test)) NA_real_ else test$p.value
  )
}

age_plot <- function(data, variable) {
  frame <- data.frame(age = data$age, value = suppressWarnings(as.numeric(data[[variable]])),
                      group_status = data$group_status)
  frame <- frame[is.finite(frame$age) & is.finite(frame$value), , drop = FALSE]
  if (!nrow(frame)) return(NULL)
  test <- one_correlation(frame$age, frame$value, "age", variable, "pearson")
  subtitle <- if (is.finite(test$estimate)) {
    sprintf("Pearson r = %.2f, p = %.3g; line = linear fit with 95%% CI", test$estimate, test$p_value)
  } else "Pearson correlation unavailable; line shown when estimable"
  score_jitter <- jitter_score_points(variable)
  point_position <- if (score_jitter) {
    ggplot2::position_jitter(width = .25, height = .12, seed = 5389)
  } else "identity"
  if (score_jitter) subtitle <- paste0(subtitle, "\nPoints lightly jittered for visibility")
  p <- ggplot2::ggplot(frame, ggplot2::aes(x = .data$age, y = .data$value)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$group_status),
                        position = point_position, alpha = .72, size = 3.2) +
    ggplot2::scale_colour_manual(values = presentation_colours, drop = FALSE) +
    ggplot2::labs(title = sprintf("%s and age (N = %d participants)", nice_label(variable), nrow(frame)),
                  subtitle = subtitle, x = "Age (years)", y = nice_label(variable), colour = "Group") +
    presentation_theme()
  if (nrow(frame) >= 3L && length(unique(frame$age)) > 1L) {
    p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                                  colour = "#202d3a", fill = "#9aafbe", linewidth = 1.6)
  }
  p
}

diagnosis_test_result <- function(data, variable) {
  values <- suppressWarnings(as.numeric(as.character(data[[variable]])))
  hc <- values[!is.na(data$group_status) & data$group_status == "HC"]
  diagnosed <- values[!is.na(data$group_status) & data$group_status == "with_diagnosis"]
  hc <- hc[is.finite(hc)]; diagnosed <- diagnosed[is.finite(diagnosed)]
  test <- if (length(hc) >= 2L && length(diagnosed) >= 2L) {
    tryCatch(stats::t.test(hc, diagnosed), error = function(e) NULL)
  } else NULL
  data.frame(
    score = variable, method = "Welch t-test", n_HC = length(hc),
    n_with_diagnosis = length(diagnosed),
    mean_HC = if (length(hc)) mean(hc) else NA_real_,
    sd_HC = if (length(hc) >= 2L) stats::sd(hc) else NA_real_,
    mean_with_diagnosis = if (length(diagnosed)) mean(diagnosed) else NA_real_,
    sd_with_diagnosis = if (length(diagnosed) >= 2L) stats::sd(diagnosed) else NA_real_,
    mean_difference_HC_minus_with_diagnosis = if (length(hc) && length(diagnosed)) {
      mean(hc) - mean(diagnosed)
    } else NA_real_,
    t_statistic = if (is.null(test)) NA_real_ else unname(test$statistic),
    df = if (is.null(test)) NA_real_ else unname(test$parameter),
    p_value = if (is.null(test)) NA_real_ else test$p.value
  )
}

format_plot_p <- function(p) {
  if (!is.finite(p)) return("unavailable")
  if (p < .001) "< .001" else sprintf("= %.3f", p)
}

diagnosis_plot <- function(data, variable, test_result) {
  frame <- data.frame(
    group = data$group_status,
    group_status = data$group_status,
    value = suppressWarnings(as.numeric(as.character(data[[variable]])))
  )
  frame <- frame[!is.na(frame$group) & frame$group %in% c("HC", "with_diagnosis") &
                   is.finite(frame$value), , drop = FALSE]
  if (!nrow(frame)) return(NULL)
  frame$group <- factor(as.character(frame$group), levels = c("HC", "with_diagnosis"))
  test_label <- if (is.finite(test_result$p_value)) {
    sprintf("Welch t(%.1f) = %.2f; p %s; Holm-adjusted p %s",
            test_result$df, test_result$t_statistic,
            format_plot_p(test_result$p_value), format_plot_p(test_result$p_holm))
  } else "Welch test unavailable (fewer than two scores in a group)"
  ggplot2::ggplot(frame, ggplot2::aes(
    x = .data$group, y = .data$value, colour = .data$group_status,
    fill = .data$group_status, group = interaction(.data$group, .data$group_status)
  )) +
    ggplot2::geom_boxplot(width = .68, linewidth = 1.25, outlier.shape = NA, alpha = .16) +
    ggplot2::geom_point(alpha = .62, size = 2.7, stroke = 0,
                        position = ggplot2::position_jitter(
                          width = .13, height = if (jitter_score_points(variable)) .12 else 0,
                          seed = 5389
                        )) +
    ggplot2::scale_colour_manual(values = presentation_colours[c("HC", "with_diagnosis")]) +
    ggplot2::scale_fill_manual(values = presentation_colours[c("HC", "with_diagnosis")]) +
    ggplot2::labs(
      title = comparison_title(frame, variable, groups = c("HC", "with_diagnosis")),
      subtitle = paste0(test_label, "; unknown excluded"),
      x = "Group", y = nice_label(variable), colour = "Group", fill = "Group"
    ) + comparison_theme()
}

score_pair_plot <- function(data, variable_x, variable_y, overall_result) {
  frame <- data.frame(
    x = suppressWarnings(as.numeric(as.character(data[[variable_x]]))),
    y = suppressWarnings(as.numeric(as.character(data[[variable_y]]))),
    group_status = data$group_status
  )
  frame <- frame[is.finite(frame$x) & is.finite(frame$y), , drop = FALSE]
  if (!nrow(frame)) return(NULL)
  subtitle <- if (is.finite(overall_result$estimate)) {
    sprintf("Overall Pearson r = %.2f; p %s; Holm-adjusted p %s",
            overall_result$estimate, format_plot_p(overall_result$p_value),
            format_plot_p(overall_result$p_holm))
  } else "Overall Pearson correlation unavailable"
  p <- ggplot2::ggplot(frame, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$group_status),
                        position = ggplot2::position_jitter(width = .12, height = .12, seed = 5389),
                        size = 3, alpha = .7) +
    ggplot2::scale_colour_manual(values = presentation_colours, drop = FALSE) +
    ggplot2::labs(
      title = sprintf("%s vs %s (N = %d)", nice_label(variable_y), nice_label(variable_x), nrow(frame)),
      subtitle = paste0(subtitle, "\nPoints jittered; fit and tests use original scores"),
      x = nice_label(variable_x), y = nice_label(variable_y), colour = "Group"
    ) + presentation_theme()
  if (nrow(frame) >= 3L && length(unique(frame$x)) > 1L && length(unique(frame$y)) > 1L) {
    p <- p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                                  colour = "#202d3a", fill = "#9aafbe", linewidth = 1.6)
  }
  p
}

score_files <- list.files(input_root, pattern = "_cognitive_scores\\.xlsx$",
                          recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
score_files <- score_files[grepl("[/\\\\]derivatives[/\\\\]experiment_data[/\\\\]", score_files)]
score_files <- score_files[!grepl("[/\\\\](old|old_data|discarded)[/\\\\]", score_files, ignore.case = TRUE)]
if (!length(score_files)) stop("No pipeline score workbooks found below ", input_root)

plot_inputs <- list()
for (input_path in score_files) {
  message("QC: ", input_path)
  scores <- as.data.frame(readxl::read_excel(input_path, sheet = "cognitive_scores"))
  if (!nrow(scores)) { warning("Empty cognitive_scores sheet: ", input_path); next }
  if (!"score_vp_id" %in% names(scores)) stop("Missing score_vp_id: ", input_path)
  if (!"sample" %in% names(scores)) stop("Missing sample: ", input_path)

  rel <- substring(normalizePath(input_path, winslash = "/"),
                   nchar(normalizePath(input_root, winslash = "/")) + 2L)
  out_dir <- file.path(output_root, dirname(rel))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  stem <- sub("\\.xlsx$", "", basename(input_path), ignore.case = TRUE)
  qc_path <- file.path(out_dir, paste0(stem, "_qc.xlsx"))
  plot_path <- file.path(out_dir, paste0(stem, "_distributions.pdf"))

  info <- get_run_info(input_path)
  input_file <- if (!is.null(info) && "input_file" %in% names(info)) info[["input_file"]] else NA_character_
  if (is.na(input_file) || !file.exists(input_file)) {
    warning("Original cognitive master unavailable; BACS trial RT QC skipped: ", input_path)
    paths <- rep(NA_character_, nrow(scores))
  } else {
    exp_label <- if ("experiment_split" %in% names(info) &&
                     !is.na(info[["experiment_split"]]) &&
                     nzchar(info[["experiment_split"]])) {
      tolower(info[["experiment_split"]])
    } else NA_character_
    master <- list(
      path = input_file,
      sample = tolower(as.character(scores$sample[1])),
      pilot = "pilot" %in% names(info) && identical(tolower(info[["pilot"]]), "true"),
      exp_label = exp_label
    )
    directories <- candidate_data_directories(master)
    if (!"BACS" %in% names(scores)) {
      warning("BACS filename column missing: ", input_path)
      paths <- rep(NA_character_, nrow(scores))
    } else {
      paths <- vapply(as.character(scores$BACS), resolve_trial_path,
                      character(1), directories = directories)
    }
  }
  # This column exists only in the separate QC result, never in the score file.
  scores$source_BACS_file <- paths
  cached <- new.env(parent = emptyenv())
  rt_details <- lapply(paths, function(path) {
    key <- if (is.na(path) || !nzchar(path)) "__MISSING__" else path
    if (!exists(key, envir = cached, inherits = FALSE)) {
      assign(key, read_bacs_rt(path), envir = cached)
    }
    get(key, envir = cached, inherits = FALSE)
  })
  raw_rt <- do.call(rbind, lapply(rt_details, `[[`, "summary"))
  qc <- cbind(
    scores[, intersect(c("sample", "score_vp_id", "source_BACS_file",
                         "Status_BACS", "Status_WCST", "Status_LNS",
                         "score_BACS_correct", "score_BACS_mean_rt",
                         "score_WCST_pers_resp", "score_WCST_pers_err",
                         "score_WCST_nonpers_err",
                         "qc_duplicate_id_in_master", "qc_BACS_training_errors",
                         "qc_BACS_trials_removed_rt", "qc_BACS_participant_rt_outlier",
                         "qc_WCST_NA_trials", "qc_LNS_NA_trials"), names(scores)), drop = FALSE],
    raw_rt
  )
  qc$qc_BACS_fast_trial_flag <- !is.na(qc$qc_BACS_prop_trials_under_400ms) &
    qc$qc_BACS_prop_trials_under_400ms >= fast_trial_proportion_flag
  qc$qc_BACS_mean_rt_over_4000ms <- if ("score_BACS_mean_rt" %in% names(scores)) {
    !is.na(scores$score_BACS_mean_rt) &
      suppressWarnings(as.numeric(scores$score_BACS_mean_rt)) > mean_rt_slow_cutoff_ms
  } else {
    rep(NA, nrow(scores))
  }
  qc$qc_BACS_fast_trials_at_least_two <- !is.na(qc$qc_BACS_trials_under_400ms) &
    qc$qc_BACS_trials_under_400ms >= 2L

  score_vars <- names(scores)[grepl("^score_(BACS|WCST|LNS)_", names(scores))]
  score_vars <- score_vars[vapply(scores[score_vars], is.numeric, logical(1))]
  distributions <- if (length(score_vars)) do.call(rbind, lapply(score_vars, function(variable) {
    one_distribution(scores[[variable]], variable)
  })) else data.frame(note = "No numeric scores available")

  # Associations between tests only; pairs from the same test are redundant.
  core_vars <- intersect(c("score_BACS_correct", "score_BACS_mean_rt",
                           "score_WCST_pers_resp", "score_WCST_pers_err",
                           "score_LNS_correct", "score_LNS_max_span"), score_vars)
  pairs <- if (length(core_vars) >= 2L) utils::combn(core_vars, 2L, simplify = FALSE) else list()
  pairs <- Filter(function(p) sub("^score_([^_]+)_.*", "\\1", p[1]) !=
                    sub("^score_([^_]+)_.*", "\\1", p[2]), pairs)
  correlations <- if (length(pairs)) do.call(rbind, lapply(pairs, function(p) {
    rbind(one_correlation(scores[[p[1]]], scores[[p[2]]], p[1], p[2], "spearman"),
          one_correlation(scores[[p[1]]], scores[[p[2]]], p[1], p[2], "pearson"))
  })) else data.frame(note = "No cross-test score pairs available")

  status_cols <- intersect(c("Status_BACS", "Status_WCST", "Status_LNS"), names(scores))
  status_counts <- if (length(status_cols)) do.call(rbind, lapply(status_cols, function(variable) {
    tab <- as.data.frame(table(ifelse(is.na(scores[[variable]]), "NA", as.character(scores[[variable]]))))
    data.frame(test = sub("^Status_", "", variable), status = tab$Var1, n = tab$Freq)
  })) else data.frame(note = "No status columns available")

  # A repeated master row must not give its BACS trials extra weight in the plot.
  all_rts <- unlist(lapply(rt_details[!duplicated(paths)], `[[`, "trials"), use.names = FALSE)
  slide_dir <- file.path(out_dir, paste0(stem, "_slides"))
  dir.create(slide_dir, recursive = TRUE, showWarnings = FALSE)
  plot_key <- paste(scores$sample, scores$score_vp_id,
                    if ("project" %in% names(scores)) scores$project else "", sep = "|")
  missing_plot_id <- is.na(scores$score_vp_id)
  plot_key[missing_plot_id] <- paste0("missing_id_row_", which(missing_plot_id))
  if (anyDuplicated(plot_key)) {
    warning("Duplicate participant IDs excluded from plots (retained in QC table): ", input_path)
  }
  scores_for_plots <- scores[!duplicated(plot_key), , drop = FALSE]
  write_plot_pdf(scores_for_plots, score_vars, all_rts, plot_path, slide_dir = slide_dir)
  plot_inputs[[input_path]] <- list(scores = scores_for_plots, trial_rts = all_rts)
  writexl::write_xlsx(list(
    qc_participants = qc,
    score_distributions = distributions,
    cross_test_correlations = correlations,
    test_status_counts = status_counts,
    settings = data.frame(
      field = c("source_scores", "rt_fast_cutoff_ms", "fast_trial_proportion_flag",
                "mean_rt_slow_cutoff_ms", "qc_changes_scores", "notes"),
      value = c(input_path, rt_fast_cutoff_ms, fast_trial_proportion_flag,
                mean_rt_slow_cutoff_ms, "FALSE",
                "QC flags are descriptive. Neither this script nor these flags exclude trials or participants.")
    )
  ), qc_path)
  message("Written: ", qc_path, " and ", plot_path)
}

# Cross-sample presentation plots: prefer the newest ALL workbook for each
# sample, to avoid counting project workbooks and ALL workbooks twice.
candidate_paths <- names(plot_inputs)
candidate_paths <- candidate_paths[!grepl("PILOT", basename(candidate_paths), ignore.case = TRUE)]
selected <- character()
for (sample_name in c("adults", "adolescents")) {
  these <- candidate_paths[vapply(candidate_paths, function(path) {
    identical(normalize_sample(plot_inputs[[path]]$scores$sample[1]), sample_name)
  }, logical(1))]
  all_scope <- these[grepl("^ALL_", basename(these), ignore.case = TRUE)]
  if (length(all_scope)) {
    dates <- vapply(all_scope, function(path) extract_date_from_name(basename(path)), numeric(1))
    selected <- c(selected, all_scope[order(dates, file.info(all_scope)$mtime,
                                             decreasing = TRUE, na.last = TRUE)[1]])
  } else if (length(these)) {
    warning("No ALL workbook for ", sample_name, "; combining project workbooks and de-duplicating IDs")
    dates <- vapply(these, function(path) extract_date_from_name(basename(path)), numeric(1))
    selected <- c(selected, these[order(dates, file.info(these)$mtime,
                                     decreasing = TRUE, na.last = TRUE)])
  }
}
if (!length(selected)) stop("No adult or adolescent cognitive scores for combined QC")

all_score_vars <- unique(unlist(lapply(selected, function(path) {
  names(plot_inputs[[path]]$scores)[grepl("^score_(BACS|WCST|LNS)_", names(plot_inputs[[path]]$scores))]
}), use.names = FALSE))

compact_scores <- function(path) {
  df <- plot_inputs[[path]]$scores
  parts <- regmatches(path, regexpr("[2-9]_backbone", path, ignore.case = TRUE))
  default_project <- if (length(parts) && nzchar(parts)) substr(parts, 1, 1) else NA_character_
  # The cognitive master often calls the project column "p". Preserve that
  # value in ALL workbooks, whose path cannot identify the individual project.
  project_values <- rep(NA_character_, nrow(df))
  for (column_name in c("project", "p")) {
    column <- which(tolower(names(df)) == column_name)
    if (length(column)) {
      candidate <- normalize_project(df[[column[1]]])
      project_values[is.na(project_values)] <- candidate[is.na(project_values)]
    }
  }
  project_values[is.na(project_values)] <- default_project
  out <- data.frame(
    sample = normalize_sample(df$sample),
    score_vp_id = as.character(df$score_vp_id),
    project = project_values,
    source_score_file = path,
    stringsAsFactors = FALSE
  )
  for (variable in all_score_vars) {
    out[[variable]] <- if (variable %in% names(df))
      suppressWarnings(as.numeric(as.character(df[[variable]]))) else rep(NA_real_, nrow(df))
  }
  out
}

combined <- do.call(rbind, lapply(selected, compact_scores))
combined$sample <- normalize_sample(combined$sample)
combined$score_vp_id <- normalize_id(combined$score_vp_id)
combined$project <- normalize_project(combined$project)
key <- paste(combined$sample, combined$score_vp_id, combined$project, sep = "|")
missing_id <- is.na(combined$score_vp_id)
key[missing_id] <- paste0("missing_id_row_", which(missing_id))
duplicated_rows <- duplicated(key)
if (any(duplicated_rows)) {
  warning(sum(duplicated_rows), " duplicate participant rows omitted from combined presentation plots")
  combined <- combined[!duplicated_rows, , drop = FALSE]
}
rownames(combined) <- NULL

combined_dir <- file.path(output_root, "combined_adults_adolescents")
dir.create(combined_dir, recursive = TRUE, showWarnings = FALSE)
combined_pdf <- file.path(combined_dir, "combined_score_distributions.pdf")
combined_rts <- unlist(lapply(selected, function(path) plot_inputs[[path]]$trial_rts), use.names = FALSE)
write_plot_pdf(combined, all_score_vars, combined_rts, combined_pdf,
               slide_dir = combined_dir)

stratification <- load_stratification(stratification_path)
if (is.null(stratification)) {
  stop("Combined distributions were written, but project/gender/age plots require stratification_info at: ",
       stratification_path)
}
combined <- join_stratification(combined, stratification)
message("Stratification matched for ", sum(combined$stratification_match != "unmatched"),
        " of ", nrow(combined), " participants")
if (any(combined$group_status == "unknown")) {
  warning(sum(combined$group_status == "unknown"),
          " participants have unknown/unrecognized group; see participant_match_audit")
}
if (any(is.na(combined$gender_status) & !is.na(combined$gender_raw))) {
  warning(sum(is.na(combined$gender_status) & !is.na(combined$gender_raw)),
          " gender values were not recognized as men/women; see participant_match_audit")
}

age_results <- list()
gender_results <- list()
available_presentation_scores <- intersect(names(presentation_scores), names(combined))
gender_tests <- setNames(lapply(available_presentation_scores, function(variable) {
  gender_test_result(combined, variable)
}), available_presentation_scores)
raw_gender_p <- vapply(gender_tests, function(result) result$p_value, numeric(1))
adjusted_gender_p <- stats::p.adjust(raw_gender_p, method = "holm")
for (variable in names(gender_tests)) {
  gender_tests[[variable]]$p_holm <- unname(adjusted_gender_p[variable])
}
diagnosis_tests <- setNames(lapply(available_presentation_scores, function(variable) {
  diagnosis_test_result(combined, variable)
}), available_presentation_scores)
raw_diagnosis_p <- vapply(diagnosis_tests, function(result) result$p_value, numeric(1))
adjusted_diagnosis_p <- stats::p.adjust(raw_diagnosis_p, method = "holm")
for (variable in names(diagnosis_tests)) {
  diagnosis_tests[[variable]]$p_holm <- unname(adjusted_diagnosis_p[variable])
}
for (variable in names(presentation_scores)) {
  if (!variable %in% names(combined)) {
    warning("Score unavailable for requested presentation plots: ", variable)
    next
  }
  for (kind in c("project", "gender", "age")) {
    p <- switch(kind,
      project = project_plot(combined, variable),
      gender = gender_plot(combined, variable, gender_tests[[variable]]),
      age = age_plot(combined, variable)
    )
    if (!is.null(p)) save_slide_plot(
      p, file.path(combined_dir, paste0(variable, "_by_", kind, ".png")),
      width = if (kind %in% c("project", "gender")) 18 else 13.333,
      height = if (kind %in% c("project", "gender")) 10 else 7.5
    )
  }
  p_diagnosis <- diagnosis_plot(combined, variable, diagnosis_tests[[variable]])
  if (!is.null(p_diagnosis)) {
    save_slide_plot(p_diagnosis, file.path(combined_dir, paste0(variable, "_by_diagnosis.png")),
                    width = 18, height = 10)
  }
  age_results[[variable]] <- do.call(rbind, lapply(c("pearson", "spearman"), function(method) {
    one_correlation(combined$age, combined[[variable]], "age", variable, method)
  }))
  for (gender in c("Men", "Women")) {
    values <- combined[[variable]][!is.na(combined$gender_status) & combined$gender_status == gender]
    values <- values[is.finite(values)]
    gender_results[[paste(variable, gender)]] <- data.frame(
      score = variable, gender = gender, n = length(values),
      mean = if (length(values)) mean(values) else NA_real_,
      sd = if (length(values) >= 2L) stats::sd(values) else NA_real_
    )
  }
}

# Pairwise associations among the four presentation scores. The overall
# analysis includes unknown group status; group-specific results use HC and
# with_diagnosis. Each Holm family consists of the six score pairs.
score_pairs <- if (length(available_presentation_scores) >= 2L) {
  utils::combn(available_presentation_scores, 2L, simplify = FALSE)
} else list()
pairwise_results <- lapply(score_pairs, function(pair) {
  do.call(rbind, lapply(c("all", "HC", "with_diagnosis"), function(group) {
    rows <- if (group == "all") combined else {
      combined[!is.na(combined$group_status) & combined$group_status == group, , drop = FALSE]
    }
    result <- do.call(rbind, lapply(c("pearson", "spearman"), function(method) {
      one_correlation(rows[[pair[1]]], rows[[pair[2]]], pair[1], pair[2], method)
    }))
    result$group <- group
    result
  }))
})
score_correlations <- if (length(pairwise_results)) do.call(rbind, pairwise_results) else {
  data.frame(note = "Fewer than two presentation scores available")
}
if (length(pairwise_results)) {
  rownames(score_correlations) <- NULL
  score_correlations$p_holm <- NA_real_
  for (group in c("all", "HC", "with_diagnosis")) {
    for (method in c("pearson", "spearman")) {
      rows <- which(score_correlations$group == group & score_correlations$method == method)
      score_correlations$p_holm[rows] <- stats::p.adjust(
        score_correlations$p_value[rows], method = "holm"
      )
    }
  }
  for (pair in score_pairs) {
    overall <- score_correlations[
      score_correlations$variable_1 == pair[1] & score_correlations$variable_2 == pair[2] &
        score_correlations$group == "all" & score_correlations$method == "pearson", , drop = FALSE
    ]
    p <- score_pair_plot(combined, pair[1], pair[2], overall)
    if (!is.null(p)) {
      save_slide_plot(p, file.path(combined_dir, paste0(pair[1], "_vs_", pair[2], "_correlation.png")))
    }
  }
}

as_sheet <- function(parts, message) {
  if (length(parts)) do.call(rbind, parts) else data.frame(note = message)
}
audit <- combined[c("sample", "score_vp_id", "project", "age", "gender_raw",
                    "gender_status", "group_raw", "group_status",
                    "qc_project_3_4_5_unknown_to_HC", "qc_project_6_hc_override",
                    "stratification_match",
                    "source_score_file")]
audit$gender_status <- as.character(audit$gender_status)
audit$group_status <- as.character(audit$group_status)
group_counts <- as.data.frame(table(
  project = ifelse(is.na(combined$project), "unknown", combined$project),
  group = combined$group_status
), stringsAsFactors = FALSE)
raw_group_counts <- as.data.frame(table(
  raw_group = ifelse(is.na(combined$group_raw), "NA", combined$group_raw),
  mapped_group = combined$group_status
), stringsAsFactors = FALSE)
raw_gender_counts <- as.data.frame(table(
  raw_gender = ifelse(is.na(combined$gender_raw), "NA", combined$gender_raw),
  mapped_gender = ifelse(is.na(combined$gender_status), "unknown/other", as.character(combined$gender_status))
), stringsAsFactors = FALSE)
writexl::write_xlsx(list(
  participant_match_audit = audit,
  project_group_counts = group_counts,
  raw_group_mapping = raw_group_counts,
  raw_gender_mapping = raw_gender_counts,
  age_correlations = as_sheet(age_results, "No requested scores available"),
  gender_descriptives = as_sheet(gender_results, "No requested scores available"),
  gender_comparisons = as_sheet(gender_tests, "No requested scores available"),
  diagnosis_comparisons = as_sheet(diagnosis_tests, "No requested scores available"),
  score_correlations = score_correlations,
  settings = data.frame(
    field = c("score_files", "stratification_path", "group_mapping",
              "project_3_4_5_group_rule", "project_6_group_rule",
              "gender_comparison", "diagnosis_comparison",
              "score_correlation"),
    value = c(paste(selected, collapse = " | "), stratification_path,
              "Unmatched or unrecognized group values remain unknown outside projects 3 to 6",
              "Unknown status in projects 3, 4, and 5 is labelled HC; explicit with_diagnosis is retained; raw group is retained in the audit",
              "All project 6 participants are labelled HC in this QC output; raw group is retained in the audit",
              "Only explicitly labelled men and women; Welch t-test is exploratory",
              "HC versus with_diagnosis excludes unknown; Welch t-test with Holm correction across available presentation scores",
              "Overall uses all pairwise complete scores (including unknown); subgroup results for HC and with_diagnosis; Holm correction within each group and method across score pairs")
  )
), file.path(combined_dir, "combined_presentation_QC.xlsx"))
message("Combined PowerPoint-ready plots and statistics written to: ", combined_dir)
