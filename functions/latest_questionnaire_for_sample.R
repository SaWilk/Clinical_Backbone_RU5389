latest_questionnaire_for_sample <- function(sample, dir_q) {
  files_all <- fs::dir_ls(dir_q, type = "file", fail = FALSE)
  patt <- glue::glue("^ALL_\\d{{4}}-\\d{{2}}-\\d{{2}}_{sample}_questionnaire\\.xlsx$")
  files <- files_all[grepl(patt, basename(files_all))]
  if (!length(files)) return(NA_character_)
  get_dt <- function(f) as.Date(stringr::str_match(basename(f), "(\\d{4}-\\d{2}-\\d{2})")[,2])
  files[order(get_dt(files), decreasing = TRUE)][1]
}