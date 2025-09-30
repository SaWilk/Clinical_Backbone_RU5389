# stand-alone probe (no dependencies on the main function)
check_meta_match <- function(sample_name, meta_env_name = "cogtest_info") {
  load_meta <- function(name) {
    # 1) R object in any parent env
    if (!is.null(name) && nzchar(name) && exists(name, inherits = TRUE)) {
      x <- get(name, inherits = TRUE)
      if (is.data.frame(x)) return(x)
    }
    # 2) name is a file path
    if (!is.null(name) && nzchar(name) && file.exists(name)) {
      ext <- tolower(tools::file_ext(name))
      if (ext %in% c("csv","tsv","txt")) {
        sep <- if (ext == "tsv") "\t" else ","
        return(utils::read.table(name, sep = sep, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE))
      } else if (ext == "rds") {
        obj <- readRDS(name)
        if (is.data.frame(obj)) return(obj)
      }
    }
    # 3) OS env var -> path
    if (!is.null(name) && nzchar(name)) {
      path <- Sys.getenv(name, unset = "")
      if (nzchar(path) && file.exists(path)) {
        ext <- tolower(tools::file_ext(path))
        if (ext %in% c("csv","tsv","txt")) {
          sep <- if (ext == "tsv") "\t" else ","
          return(utils::read.table(path, sep = sep, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE))
        } else if (ext == "rds") {
          obj <- readRDS(path)
          if (is.data.frame(obj)) return(obj)
        }
      }
    }
    NULL
  }
  
  norm_tok <- function(x) tolower(gsub("[^a-z0-9]+", "", trimws(as.character(x))))
  extract_ymd <- function(x) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_character_)
    x_chr <- as.character(x[1])
    m <- regexpr("\\d{4}-\\d{2}-\\d{2}", x_chr)
    if (m[1] != -1) return(substr(x_chr, m[1], m[1]+attr(m,"match.length")-1))
    suppressWarnings({
      ts <- try(as.POSIXct(x_chr, tz = "UTC"), silent = TRUE)
      if (!inherits(ts, "try-error") && !is.na(ts)) return(format(ts, "%Y-%m-%d"))
      d  <- try(as.Date(x_chr), silent = TRUE)
      if (!inherits(d, "try-error") && !is.na(d)) return(format(d, "%Y-%m-%d"))
    })
    NA_character_
  }
  
  meta_df <- load_meta(meta_env_name)
  if (is.null(meta_df)) return(list(status="no_metadata", date_tag="unknown_date", rows_matched=integer(0)))
  
  if (!all(c("sample","ctime") %in% names(meta_df)))
    return(list(status="missing_columns", date_tag="unknown_date", rows_matched=integer(0)))
  
  meta_sample_norm <- norm_tok(meta_df$sample)
  needle <- norm_tok(sample_name)
  rows <- which(meta_sample_norm == needle)
  if (!length(rows)) rows <- which(grepl(needle, meta_sample_norm, fixed = TRUE))
  
  if (!length(rows)) return(list(status="no_match", date_tag="unknown_date", rows_matched=integer(0)))
  
  for (ct in meta_df$ctime[rows]) {
    ymd <- extract_ymd(ct)
    if (!is.na(ymd) && nzchar(ymd)) {
      return(list(status="ok", date_tag=ymd, rows_matched=rows))
    }
  }
  list(status="no_parseable_date", date_tag="unknown_date", rows_matched=rows)
}
