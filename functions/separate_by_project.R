# Splitter for cognitive tests (prefers "project"; supports legacy names)
# Project formats handled:
#   A) "P 123 (names...)"   -> general_info_<sample>
#   B) "123" (just number)  -> <pid>_<date>_<sample>   (skip saving pid 0/99)
separate_by_project <- function(df, out_path = NULL, sample = NULL, export_csv = FALSE) {
  # ------------- setup -------------
  if (is.null(out_path)) out_path <- get0("out_path", inherits = TRUE)
  if (is.null(out_path)) stop("Please define 'out_path' in your environment or pass it as an argument.")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required. Install it with install.packages('writexl').")
  }
  
  # ------------- find project column -------------
  proj_cols <- c("project", "Projekt...Project", "Projekt.", "Projekt", "projekt", "p", "proj", "Proj")
  lower_names <- tolower(names(df))
  match_idx <- which(tolower(proj_cols) %in% lower_names)[1]
  if (is.na(match_idx)) {
    stop(
      "Could not find a project column. Tried: ",
      paste(proj_cols, collapse = ", "),
      ". Found: ", paste(names(df), collapse = ", "), "."
    )
  }
  proj_col <- names(df)[match(tolower(proj_cols[match_idx]), lower_names)]
  message("separate_by_project: using column '", proj_col, "'")
  
  # normalize + drop empties/testing rows
  df[[proj_col]] <- trimws(as.character(df[[proj_col]]))
  df <- df[!(df[[proj_col]] %in% c("", "Testing mode(No actual data collection)")), , drop = FALSE]
  
  # ------------- robust SAMPLE detection -------------
  normalize_sample <- function(x) {
    x <- tolower(x)
    # prefer explicit children_parents over children
    if (grepl("children_parents", x)) return("children_parents")
    if (grepl("(?:^|[_\\.-])(children)(?:$|[_\\.-])", x)) return("children_parents")
    if (grepl("(?:^|[_\\.-])(adolescents)(?:$|[_\\.-])", x)) return("adolescents")
    if (grepl("(?:^|[_\\.-])(adults)(?:$|[_\\.-])", x)) return("adults")
    "unknown"
  }
  
  # pull the *text* of the df argument from the call site, then parse the sample from that text
  infer_sample_from_call <- function() {
    # (1) from substitute text
    txt1 <- tryCatch(paste(deparse(substitute(df)), collapse = ""), error = function(e) "")
    s1 <- normalize_sample(txt1); if (s1 != "unknown") return(s1)
    
    # (2) from match.call (sometimes cleaner)
    mc <- tryCatch(match.call(expand.dots = FALSE)$df, error = function(e) NULL)
    txt2 <- tryCatch(if (!is.null(mc)) paste(deparse(mc), collapse = "") else "", error = function(e) "")
    s2 <- normalize_sample(txt2); if (s2 != "unknown") return(s2)
    
    # (3) last ditch: look at parent frame names (don’t rely on identical(), just names)
    pf <- parent.frame()
    nms <- ls(envir = pf, all.names = TRUE)
    # prefer names that contain known sample tokens
    cand <- nms[grepl("adults|adolescents|children_parents|children", tolower(nms))]
    for (nm in cand) {
      s3 <- normalize_sample(nm)
      if (s3 != "unknown") return(s3)
    }
    
    "unknown"
  }
  
  sample_name <- if (!is.null(sample)) normalize_sample(sample) else infer_sample_from_call()
  if (sample_name == "unknown") {
    warning("Could not infer sample from the call. Consider passing sample = 'adults'/'adolescents'/'children_parents'.")
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
  
  # ------------- split & write -------------
  parts <- split(df, df[[proj_col]], drop = TRUE)
  proj_labels <- names(parts)
  
  is_empty_df <- function(x) {
    is.null(x) || nrow(x) == 0 || all(vapply(x, function(col) all(is.na(col)), logical(1)))
  }
  
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  
  for (lbl in proj_labels) {
    d <- parts[[lbl]]
    parsed <- parse_project(lbl)
    pid <- parsed$pid
    kind <- parsed$type
    
    if (kind == "general_info") {
      # env: data_general_info_<sample>
      varname <- sprintf("data_general_info_%s", sample_name)
      assign(varname, d, envir = .GlobalEnv)
      
      # file: general_info_<date>_<sample>.(xlsx/csv)
      base <- sprintf("general_info_%s_%s", date_str, sample_name)
      if (export_csv) utils::write.csv(d, file.path(out_path, paste0(base, ".csv")), row.names = FALSE, na = "")
      writexl::write_xlsx(d, file.path(out_path, paste0(base, ".xlsx")))
      
    } else {
      pid_clean <- if (!is.na(pid) && nzchar(pid)) pid else "unknown"
      
      # env: data_<sample>_p_<pid>
      varname <- sprintf("data_%s_p_%s", sample_name, pid_clean)
      assign(varname, d, envir = .GlobalEnv)
      
      # file: <pid>_<date>_<sample>, but skip pid 0/99
      suppress_save <- !is.na(pid_clean) && pid_clean %in% c("0", "99")
      if (!suppress_save) {
        base <- sprintf("%s_%s_%s", pid_clean, date_str, sample_name)
        if (export_csv) utils::write.csv(d, file.path(out_path, paste0(base, ".csv")), row.names = FALSE, na = "")
        writexl::write_xlsx(d, file.path(out_path, paste0(base, ".xlsx")))
      } else {
        message("Skipping disk save for test project pid=", pid_clean, " (kept in environment as ", varname, ").")
      }
    }
  }
  
  invisible(TRUE)
}
