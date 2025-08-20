# Extract pilot subjects by vpid and remove them from the dataset
# - You can define pilots via exact IDs (pilot_ids) or a regex (pilot_regex).
# - If neither is provided, we use a conservative default regex that matches
#   vpid values starting with "pilot" or "pil" (case-insensitive). If nothing
#   matches, no rows are moved.
#
# Returns the dataset *without* the pilot rows (so you can reassign),
# and also creates an env var 'pilot_<sample>' with the pilot rows.
#
# Example:
#   dat_adults <- extract_pilot_by_vpid(dat_adults, out_path="out", pilot_regex="^99\\d+$", sample="adults")
#   dat_children_parents <- extract_pilot_by_vpid(dat_children_parents, out_path="out",
#                                                 pilot_ids=c("9001","9002"), sample="children_parents")
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
  id_cols <- c("vpid", "Versuchspersonennummer.", "Versuchspersonen.ID...Participant.ID", "id", "vpid_1")
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
  if (is.null(sample)) {
    # best-effort: parse the text of the df argument
    txt <- tryCatch(paste(deparse(substitute(df)), collapse = ""), error = function(e) "")
    sample <- normalize_sample(txt)
  } else {
    sample <- normalize_sample(sample)
  }
  
  # ---- choose pilot matcher ----
  # Priority: exact IDs > regex > conservative default (matches "pilot"/"pil" prefixes)
  if (!is.null(pilot_ids)) {
    pilot_mask <- vpid %in% as.character(pilot_ids)
  } else {
    if (is.null(pilot_regex) || !nzchar(pilot_regex)) {
      pilot_regex <- "(?i)^(pilot|pil)\\b"  # safe default; matches none in many datasets
    }
    pilot_mask <- grepl(pilot_regex, vpid, perl = TRUE)
  }
  
  # ---- split ----
  pilot_df <- df[pilot_mask, , drop = FALSE]
  main_df  <- df[!pilot_mask, , drop = FALSE]
  
  # ---- assign env var + save to disk (one sheet/file) ----
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  # env var: pilot_<sample>
  env_name <- sprintf("pilot_%s", sample)
  assign(env_name, pilot_df, envir = .GlobalEnv)
  
  # file name: pilot_<date>_<sample>.(xlsx/csv)
  base <- sprintf("pilot_%s_%s", date_str, sample)
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
    "', moved ", nrow(pilot_df), " pilot rows; keeping ", nrow(main_df), " rows."
  )
  return(main_df)
}
