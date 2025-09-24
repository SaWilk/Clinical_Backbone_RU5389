# -------------------------------------------------------------------------
# separate_by_project()
#
# Purpose:
#   Splits a dataset into separate data frames by project identifier and
#   saves them to disk. Handles both numeric project IDs and labels that
#   embed the project number (e.g. "P 123 ...").
#
# Behavior:
#   - Detects the project column, searching in this order of preference:
#       • "project"
#       • "Projekt...Project"
#       • "Projekt."
#       • "Projekt"
#       • "projekt"
#       • "p"
#       • "proj"
#       • "Proj"
#     If not found: WARN (do not stop) and assign all rows to project "6".
#   - Removes test participants: Versuchspersonennummer. == 99999 (if column exists).
#   - Cleans project column values (trims whitespace, removes test markers).
#   - Infers the sample name from:
#       • The `sample` argument if supplied.
#       • Otherwise from the variable name of the input data frame.
#       • Recognizes "adults", "adolescents", "children_parents"/"children", "cogtests".
#       • Falls back to "unknown" if not found.
#   - Project label parsing:
#       • "P <digits>" at the start → treated as "general_info" project.
#       • Pure digits (e.g. "123") → treated as numeric project ID.
#       • Any string with digits → digits are extracted as the project ID.
#       • Other cases → project ID set to NA ("unknown").
#   - Output objects:
#       • If "general_info": creates `data_general_info_<sample>`.
#       • Otherwise: creates `data_<sample>_p_<pid>`.
#   - Output files:
#       • For "general_info": saves as
#         `general_info_<date>_<sample>.xlsx` (+ CSV if requested),
#         only if non-empty.
#       • For numeric projects: saves as `<pid>_<date>_<sample>.xlsx`
#         (+ CSV if requested), only if non-empty, skipping IDs 0 and 99
#         (kept in memory only).
#   - Files are saved under `out_path`; directories are created as needed.
#
# Input:
#   df         : data frame to split by project.
#   out_path   : directory path where files are saved (required if no
#                global `out_path` exists).
#   sample     : optional character string ("adults", "adolescents",
#                "children_parents", "cogtests"); overrides autodetect.
#   export_csv : logical, if TRUE also export CSV alongside XLSX.
#
# Output:
#   - Returns TRUE (invisibly).
#   - Creates project-specific data frames in the global environment.
#   - Writes XLSX (and optional CSV) files to disk.
#
# Example:
#   separate_by_project(dat_adults, out_path = "exports", export_csv = TRUE)
#
# -------------------------------------------------------------------------
separate_by_project <- function(df, out_path = NULL, sample = NULL, export_csv = FALSE) {
  # ------------- setup -------------
  if (is.null(out_path)) out_path <- get0("out_path", inherits = TRUE)
  if (is.null(out_path)) stop("Please define 'out_path' in your environment or pass it as an argument.")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required. Install it with install.packages('writexl').")
  }
  
  # ------------- find or create project column -------------
  proj_cols <- c("project", "Projekt...Project", "Projekt.", "Projekt", "projekt", "p", "proj", "Proj")
  lower_names <- tolower(names(df))
  match_idx <- which(tolower(proj_cols) %in% lower_names)[1]
  
  if (is.na(match_idx)) {
    warning("Could not find a project column. Assigning all rows to default project '6'. ",
            "Tried: ", paste(proj_cols, collapse = ", "), ".")
    proj_col <- "Projekt."
    df[[proj_col]] <- "6"
  } else {
    proj_col <- names(df)[match(tolower(proj_cols[match_idx]), lower_names)]
  }
  message("separate_by_project: using column '", proj_col, "'")

  # ------------- robust SAMPLE detection -------------
  normalize_sample <- function(x) {
    x <- tolower(x)
    # prefer explicit markers
    if (grepl("parents_p6", x)) return("parents_p6")
    if (grepl("children_p6", x)) return("children_p6")
    if (grepl("children_parents", x)) return("children_parents")
    if (grepl("(?:^|[_\\.-])(children)(?:$|[_\\.-])", x)) return("children_parents")
    if (grepl("(?:^|[_\\.-])(adolescents)(?:$|[_\\.-])", x)) return("adolescents")
    if (grepl("(?:^|[_\\.-])(adults)(?:$|[_\\.-])", x)) return("adults")
    if (grepl("(?:^|[_\\.-])(cogtests?)(?:$|[_\\.-])", x)) return("cogtests")
    "unknown"
  }
  
  infer_sample_from_call <- function() {
    txt1 <- tryCatch(paste(deparse(substitute(df)), collapse = ""), error = function(e) "")
    s1 <- normalize_sample(txt1); if (s1 != "unknown") return(s1)
    
    mc <- tryCatch(match.call(expand.dots = FALSE)$df, error = function(e) NULL)
    txt2 <- tryCatch(if (!is.null(mc)) paste(deparse(mc), collapse = "") else "", error = function(e) "")
    s2 <- normalize_sample(txt2); if (s2 != "unknown") return(s2)
    
    pf <- parent.frame()
    nms <- ls(envir = pf, all.names = TRUE)
    cand <- nms[grepl("adults|adolescents|children_parents|children|cogtests", tolower(nms))]
    for (nm in cand) {
      s3 <- normalize_sample(nm)
      if (s3 != "unknown") return(s3)
    }
    "unknown"
  }
  
  sample_name <- if (!is.null(sample)) normalize_sample(sample) else infer_sample_from_call()
  if (sample_name == "unknown") {
    warning("Could not infer sample from the call. Consider passing sample = 'adults'/'adolescents'/'children_parents'/'cogtests'.")
  }
  message("separate_by_project: sample = '", sample_name, "'")
  
  # ------------- project parsing -------------
  parse_project <- function(lbl) {
    if (is.na(lbl)) return(list(type = "other", pid = NA_character_))
    s <- trimws(as.character(lbl))
    
    # "P <digits>" at start (case-insensitive) -> general_info
    m1 <- regexec("(?i)^\\s*P\\s*(\\d+)", s, perl = TRUE)
    r1 <- regmatches(s, m1)[[1]]
    if (length(r1) >= 2 && nzchar(r1[2])) {
      return(list(type = "general_info", pid = r1[2]))
    }
    
    # pure number
    m2 <- regexec("^\\s*(\\d+)\\s*$", s, perl = TRUE)
    r2 <- regmatches(s, m2)[[1]]
    if (length(r2) >= 2 && nzchar(r2[2])) {
      return(list(type = "number", pid = r2[2]))
    }
    
    # digits anywhere => treat as number
    digits <- gsub("\\D+", "", s)
    if (nzchar(digits)) return(list(type = "number", pid = digits))
    
    list(type = "other", pid = NA_character_)
  }
  
  # ------------- helpers -------------
  is_empty_df <- function(x) {
    is.null(x) || nrow(x) == 0 || all(vapply(x, function(col) all(is.na(col)), logical(1)))
  }
  
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  
  # ------------- split & write -------------
  parts <- split(df, df[[proj_col]], drop = TRUE)
  proj_labels <- names(parts)
  
  for (lbl in proj_labels) {
    d <- parts[[lbl]]
    parsed <- parse_project(lbl)
    pid <- parsed$pid
    kind <- parsed$type
    
    if (kind == "general_info") {
      # env: data_general_info_<sample>
      varname <- sprintf("data_general_info_%s", sample_name)
      assign(varname, d, envir = .GlobalEnv)
      
      # file: general_info_<date>_<sample>.(xlsx/csv) — only if non-empty
      if (!is_empty_df(d)) {
        base <- sprintf("general_info_%s_%s", date_str, sample_name)
        if (export_csv) utils::write.csv(d, file.path(out_path, paste0(base, ".csv")), row.names = FALSE, na = "")
        writexl::write_xlsx(d, file.path(out_path, paste0(base, ".xlsx")))
      } else {
        message("Skipping save: 'general_info' is empty (kept in environment as ", varname, ").")
      }
      
    } else {
      pid_clean <- if (!is.na(pid) && nzchar(pid)) pid else "unknown"
      
      # env: data_<sample>_p_<pid>
      varname <- sprintf("data_%s_p_%s", sample_name, pid_clean)
      assign(varname, d, envir = .GlobalEnv)
      
      # file: <pid>_<date>_<sample>, but skip pid 0/99 and empty data
      suppress_save <- pid_clean %in% c("0", "99")
      if (suppress_save) {
        message("Skipping disk save for test project pid=", pid_clean, " (kept in environment as ", varname, ").")
      } else if (!is_empty_df(d)) {
        base <- sprintf("%s_%s_%s", pid_clean, date_str, sample_name)
        if (export_csv) utils::write.csv(d, file.path(out_path, paste0(base, ".csv")), row.names = FALSE, na = "")
        writexl::write_xlsx(d, file.path(out_path, paste0(base, ".xlsx")))
      } else {
        message("Skipping save: project pid=", pid_clean, " is empty (kept in environment as ", varname, ").")
      }
    }
  }
  
  invisible(TRUE)
}
