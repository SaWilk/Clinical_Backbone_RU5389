#!/usr/bin/env Rscript

# Install/load packages --------------------------------------------------------
suppressPackageStartupMessages({
  if (!require("dplyr"))     { install.packages("dplyr")      }; library(dplyr)
  if (!require("tidyr"))     { install.packages("tidyr")      }; library(tidyr)
  if (!require("readxl"))    { install.packages("readxl")     }; library(readxl)
  if (!require("openxlsx"))  { install.packages("openxlsx")   }; library(openxlsx)
  if (!require("purrr"))     { install.packages("purrr")      }; library(purrr)
  if (!require("stringr"))   { install.packages("stringr")    }; library(openxlsx)
})

# -------------------- Helpers & Config --------------------

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  path <- sub(file_arg, "", args[grepl(file_arg, args)])
  if (length(path)) dirname(normalizePath(path)) else getwd()
}

SCRIPT_DIR <- get_script_dir()
DATA_DIR   <- file.path(SCRIPT_DIR, "private_information")
PROJ_ROOT  <- file.path(SCRIPT_DIR, "01_project_data")

if (!dir.exists(DATA_DIR)) stop("Directory not found: ", DATA_DIR)

# Ensure project root exists (we'll create subfolders as needed)
if (!dir.exists(PROJ_ROOT)) dir.create(PROJ_ROOT, recursive = TRUE, showWarnings = FALSE)

TYPES <- c("cogtest", "questionnaire", "questionnaire_p6")

FILENAME_RE <- "^(.*?)_ids_in_all_projects_(cogtest|questionnaire|questionnaire_p6)\\.xlsx$"

RE_COG  <- "^(?:psytool_info|cogtest)_([A-Za-z0-9_]+)_([1-9])$"
RE_Q    <- "^(?:dat|questionnaire)_([A-Za-z0-9_]+)_([1-9])$"
RE_QP6  <- "^(?:dat|questionnaire)_([A-Za-z0-9_]+)_p([1-9])$"

normalize_sample <- function(s) {
  s <- tolower(trimws(s))
  if (s %in% c("children_parents", "parents")) return("children")
  s
}

parse_date_tag <- function(tag) {
  fmts <- c("%Y-%m-%d", "%Y%m%d", "%d-%m-%Y", "%Y.%m.%d")
  for (fmt in fmts) {
    dt <- try(as.POSIXct(tag, format = fmt, tz = "UTC"), silent = TRUE)
    if (!inherits(dt, "try-error") && !is.na(dt)) {
      return(list(key = as.numeric(dt), norm = format(dt, "%Y-%m-%d")))
    }
  }
  if (grepl("^[0-9]+$", tag)) return(list(key = as.numeric(tag), norm = tag))
  list(key = NA_real_, norm = tag)
}

# -------------------- Discover latest files --------------------

files <- list.files(DATA_DIR, pattern = "_ids_in_all_projects_.*\\.xlsx$", full.names = TRUE)
if (!length(files)) stop("No matching files in ", DATA_DIR)

meta <- tibble(
  path = files,
  name = basename(files)
) %>%
  mutate(m = str_match(name, FILENAME_RE)) %>%
  filter(!is.na(m[,1])) %>%
  transmute(
    path,
    dtype = tolower(m[,3]),
    date_tag = m[,2]
  ) %>%
  rowwise() %>%
  mutate(
    parsed = list(parse_date_tag(date_tag)),
    date_key = parsed$key,
    date_norm = parsed$norm
  ) %>%
  ungroup()

if (!nrow(meta)) stop("No files matched the expected naming pattern.")

coverage <- meta %>%
  group_by(date_norm) %>%
  summarize(
    types_present = n_distinct(dtype),
    max_key = max(coalesce(date_key, -Inf)),
    .groups = "drop"
  ) %>%
  arrange(desc(types_present), desc(max_key))

best_norm <- coverage$date_norm[1]

chosen <- meta %>%
  filter(date_norm == best_norm) %>%
  group_by(dtype) %>%
  slice_max(order_by = coalesce(date_key, -Inf), with_ties = FALSE) %>%
  ungroup()

if (!"cogtest" %in% chosen$dtype) {
  newest_cog <- meta %>%
    filter(dtype == "cogtest") %>%
    slice_max(order_by = coalesce(date_key, -Inf), with_ties = FALSE)
  if (!nrow(newest_cog)) stop("Could not find any cogtest file.")
  chosen <- bind_rows(chosen, newest_cog)
}

message("Using files:")
chosen %>%
  arrange(factor(dtype, levels = TYPES)) %>%
  { purrr::pwalk(list(..1=.$path, ..2=.$dtype), ~ message("  - ", ..2, ": ", basename(..1))) }

paths <- setNames(chosen$path, chosen$dtype)

# ---- capture the exact date tag from the chosen cogtest file for filenames ----
cog_date_tag <- chosen %>% filter(dtype == "cogtest") %>% pull(date_tag) %>% .[1]
if (is.na(cog_date_tag) || length(cog_date_tag) == 0) {
  # fall back to normalized date if needed
  cog_date_tag <- chosen %>% filter(dtype == "cogtest") %>% pull(date_norm) %>% .[1]
}

# -------------------- Load and extract IDs per (sample, project) --------------------

read_if <- function(dtype) if (dtype %in% names(paths)) readxl::read_excel(paths[[dtype]]) else NULL

collect_ids_from_cols <- function(df, pattern, map_project = identity, sample_normalizer = normalize_sample) {
  if (is.null(df)) return(list())
  out <- list()
  for (cn in names(df)) {
    if (!is.character(cn)) next
    m <- str_match(cn, pattern)
    if (is.na(m[1,1])) next
    sample <- sample_normalizer(m[1,2])
    project <- as.integer(map_project(m[1,3]))
    ids <- df[[cn]] %>% as.character() %>% trimws() %>% .[!is.na(.) & . != ""]
    key <- paste(sample, project, sep = "|")
    if (!length(ids)) next
    if (is.null(out[[key]])) out[[key]] <- unique(ids) else out[[key]] <- union(out[[key]], ids)
  }
  out
}

cog_df  <- read_if("cogtest")
q_df    <- read_if("questionnaire")
qp6_df  <- read_if("questionnaire_p6")

cog_map <- collect_ids_from_cols(cog_df, RE_COG)
q_map   <- collect_ids_from_cols(q_df, RE_Q)
qp6_map <- collect_ids_from_cols(qp6_df, RE_QP6, map_project = function(x) as.integer(x))

merge_maps <- function(a, b) {
  keys <- union(names(a), names(b))
  out <- vector("list", length(keys)); names(out) <- keys
  for (k in keys) out[[k]] <- union(a[[k]] %||% character(), b[[k]] %||% character())
  out
}
`%||%` <- function(x, y) if (is.null(x)) y else x

q_all <- merge_maps(q_map, qp6_map)

valid_key <- function(k) {
  parts <- strsplit(k, "\\|")[[1]]
  if (length(parts) != 2) return(FALSE)
  p <- suppressWarnings(as.integer(parts[2]))
  !is.na(p) && p >= 1 && p <= 9
}

all_keys <- union(names(cog_map), names(q_all))
all_keys <- all_keys[vapply(all_keys, valid_key, logical(1))]
all_keys <- sort(all_keys)

rows_complete <- list()
rows_missing_q <- list()
rows_missing_c <- list()

for (k in all_keys) {
  parts <- strsplit(k, "\\|")[[1]]
  sample <- parts[1]; project <- as.integer(parts[2])
  
  cog_ids <- cog_map[[k]] %||% character()
  q_ids   <- q_all[[k]]   %||% character()
  
  complete   <- intersect(cog_ids, q_ids)
  missing_q  <- setdiff(cog_ids, q_ids)
  missing_c  <- setdiff(q_ids, cog_ids)
  
  if (length(complete))     rows_complete[[k]] <- tibble(sample, project, id = sort(unique(complete)))
  if (length(missing_q))    rows_missing_q[[k]] <- tibble(sample, project, id = sort(unique(missing_q)))
  if (length(missing_c))    rows_missing_c[[k]] <- tibble(sample, project, id = sort(unique(missing_c)))
}

df_complete <- if (length(rows_complete)) bind_rows(rows_complete) else tibble(sample=character(), project=integer(), id=character())
df_miss_q   <- if (length(rows_missing_q)) bind_rows(rows_missing_q) else tibble(sample=character(), project=integer(), id=character())
df_miss_c   <- if (length(rows_missing_c)) bind_rows(rows_missing_c) else tibble(sample=character(), project=integer(), id=character())

# -------------------- Write OVERVIEW workbook (date-prefixed) --------------------

overview_name <- sprintf("%s_id_completeness_report.xlsx", cog_date_tag)
overview_path <- file.path(DATA_DIR, overview_name)

openxlsx::write.xlsx(
  list(
    complete = df_complete,
    missing_questionnaire = df_miss_q,
    missing_cogtest = df_miss_c
  ),
  file = overview_path,
  overwrite = TRUE
)

# -------------------- Write per-project subtables into 01_project_data/<n>_backbone --------------------

write_project_workbook <- function(prj, df_complete, df_miss_q, df_miss_c) {
  # Skip creating an XLSX for project 1 
  if (prj == 1) { return(invisible(NULL)) }

  sub_complete <- df_complete %>% filter(project == prj)
  sub_miss_q   <- df_miss_q   %>% filter(project == prj)
  sub_miss_c   <- df_miss_c   %>% filter(project == prj)
  
  # Destination folder
  prj_dir <- file.path(PROJ_ROOT, sprintf("%d_backbone", prj))
  if (!dir.exists(prj_dir)) dir.create(prj_dir, recursive = TRUE, showWarnings = FALSE)
  
  prj_name <- sprintf("%s_project_%d_id_completeness.xlsx", cog_date_tag, prj)
  prj_path <- file.path(prj_dir, prj_name)
  
  wb <- createWorkbook()
  addWorksheet(wb, "complete")
  addWorksheet(wb, "missing_questionnaire")
  addWorksheet(wb, "missing_cogtest")
  
  writeData(wb, "complete", sub_complete)
  writeData(wb, "missing_questionnaire", sub_miss_q)
  writeData(wb, "missing_cogtest", sub_miss_c)
  
  saveWorkbook(wb, prj_path, overwrite = TRUE)
  invisible(prj_path)
}

prj_paths <- lapply(1:9, write_project_workbook, df_complete=df_complete, df_miss_q=df_miss_q, df_miss_c=df_miss_c)

# -------------------- Console summary --------------------

summarize_counts <- function(df) {
  if (!nrow(df)) return("—")
  df %>%
    count(sample, project, name = "n") %>%
    arrange(sample, project) %>%
    mutate(txt = paste0(sample, " / ", project, ": ", n)) %>%
    pull(txt) %>%
    paste(collapse = "; ")
}

cat("Overview written to:", overview_path, "\n\n")
cat("Counts per (sample, project):\n")
cat("  complete:              ", summarize_counts(df_complete), "\n", sep = "")
cat("  missing_questionnaire: ", summarize_counts(df_miss_q),   "\n", sep = "")
cat("  missing_cogtest:       ", summarize_counts(df_miss_c),   "\n", sep = "")
cat("\nProject workbooks:\n")
for (i in seq_along(prj_paths)) cat("  - ", prj_paths[[i]], "\n", sep = "")
cat("\nUsing files:\n")
chosen %>%
  arrange(factor(dtype, levels = TYPES)) %>%
  mutate(line = paste0("  - ", dtype, ": ", basename(path))) %>%
  pull(line) %>%
  cat(paste0(., collapse = "\n"), "\n")
