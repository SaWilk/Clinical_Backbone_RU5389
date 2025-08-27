extract_pilot_by_vpid <- function(
    df,
    out_path = NULL,
    export_csv = FALSE,
    sample = NULL,
    pilot_ids = NULL,
    pilot_regex = NULL
) {
  # ---- setup out_path & dependency ----
  if (is.null(out_path)) out_path <- get0("out_path", inherits = TRUE)
  if (is.null(out_path)) stop("Please define 'out_path' in your environment or pass it as an argument.")
  if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE, showWarnings = FALSE)
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("Package 'writexl' is required. Install it with install.packages('writexl').")
  }
  
  # ---- find vpid column (new + legacy) ----
  # keep 'id' in the candidates so PsyToolkit exports work out of the box
  id_cols <- c("id", "vpid", "Versuchspersonennummer.", "Versuchspersonen.ID...Participant.ID", "vpid_1")
  id_col <- intersect(id_cols, names(df))[1]
  if (is.na(id_col)) stop("No participant ID column found. Tried: ", paste(id_cols, collapse = ", "))
  
  # ensure character
  vpid <- trimws(as.character(df[[id_col]]))
  
  # ---- infer sample if not given (from the name used in the call) ----
  normalize_sample <- function(x) {
    x <- tolower(x)
    if (grepl("children_parents", x)) return("children_parents")
    if (grepl("children", x)) return("children_parents")  # legacy -> new
    if (grepl("adolescents", x)) return("adolescents")
    if (grepl("adults", x)) return("adults")
    "unknown"
  }
  txt <- tryCatch(paste(deparse(substitute(df)), collapse = ""), error = function(e) "")
  if (is.null(sample)) {
    sample <- normalize_sample(txt)
  } else {
    sample <- normalize_sample(sample)
  }
  
  # ---- detect PsyToolkit exports for naming only ----
  looks_psytool <- ("p" %in% names(df) && "id" %in% names(df)) || grepl("psytool_info", tolower(txt), fixed = TRUE)
  
  # ---- choose pilot matcher (priority: exact IDs > regex > conservative default) ----
  if (!is.null(pilot_ids)) {
    pilot_mask <- vpid %in% as.character(pilot_ids)
    match_src <- sprintf("exact IDs on '%s'", id_col)
  } else if (!is.null(pilot_regex) && nzchar(pilot_regex)) {
    pilot_mask <- grepl(pilot_regex, vpid, perl = TRUE)
    match_src <- sprintf("vpid regex on '%s'", id_col)
  } else {
    pilot_mask <- grepl("(?i)^(pilot|pil)\\b", vpid, perl = TRUE)  # conservative default
    match_src <- sprintf("default vpid regex on '%s'", id_col)
  }
  
  # ---- split ----
  pilot_df <- df[pilot_mask, , drop = FALSE]
  main_df  <- df[!pilot_mask, , drop = FALSE]
  
  # ---- assign env var + save to disk (one sheet/file) ----
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  env_name <- sprintf("pilot_%s", sample)
  assign(env_name, pilot_df, envir = .GlobalEnv)
  
  # filename: add "psytool_info" midfix if this is PsyToolkit data
  base <- if (looks_psytool) {
    sprintf("pilot_%s_psytool_info_%s", date_str, sample)
  } else {
    sprintf("pilot_%s_%s", date_str, sample)
  }
  
  if (nrow(pilot_df) > 0) {
    if (export_csv) {
      utils::write.csv(pilot_df, file.path(out_path, paste0(base, ".csv")), row.names = FALSE, na = "")
    }
    writexl::write_xlsx(pilot_df, file.path(out_path, paste0(base, ".xlsx")))
  } else {
    message("No pilot rows matched; no file written. (Env var ", env_name, " still assigned with 0 rows.)")
  }
  
  # ---- messaging & return ----
  message(
    "extract_pilot_by_vpid: sample='", sample, "', vpid_col='", id_col,
    "', moved ", nrow(pilot_df), " pilot rows; keeping ", nrow(main_df), " rows. Matcher: ", match_src, "."
  )
  return(main_df)
}
