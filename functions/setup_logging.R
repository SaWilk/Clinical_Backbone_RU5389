setup_logging <- function(log_path = "logs/all_action_points.log",
                          append = FALSE,
                          with_timestamp = TRUE,
                          enforce_code = TRUE) {
  
  ## internal logger factory
  create_logger <- function(log_path,
                            append = FALSE,
                            with_timestamp = TRUE,
                            enforce_code = TRUE) {
    dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)
    con <- file(log_path, open = if (append) "a" else "w", blocking = TRUE)
    
    write_line <- function(text) {
      if (enforce_code && !grepl("\\b\\d{5}\\b", text)) {
        stop("Each log line must contain a 5-digit code (e.g., 01234). Found: ", text)
      }
      ts <- if (with_timestamp) paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ") else ""
      line <- paste0(ts, text)
      writeLines(line, con = con)
      flush(con)
      invisible(line)
    }
    
    split_into_sublogs <- function(dest_map,
                                   pattern = "\\b\\d{5}\\b",
                                   file_namer = function(d) sprintf("sublog_%s.log", d),
                                   overwrite = TRUE) {
      if (is.null(names(dest_map)) || any(!names(dest_map) %in% as.character(0:9))) {
        stop("dest_map must be a named vector/list with names '0'..'9'")
      }
      if (!file.exists(log_path)) stop("Log file not found: ", log_path)
      
      lines <- readLines(log_path, warn = FALSE)
      buckets <- setNames(vector("list", 10), as.character(0:9))
      
      for (ln in lines) {
        m <- regexpr(pattern, ln)
        if (m[1] > 0) {
          code <- substr(ln, m[1], m[1] + attr(m, "match.length") - 1)
          first_digit <- substr(code, 1, 1)
          if (!is.null(dest_map[[first_digit]])) {
            buckets[[first_digit]] <- c(buckets[[first_digit]], ln)
          }
        }
      }
      
      out_paths <- list()
      for (d in names(dest_map)) {
        shard_lines <- buckets[[d]]
        if (length(shard_lines)) {
          dir.create(dest_map[[d]], recursive = TRUE, showWarnings = FALSE)
          out_file <- file.path(dest_map[[d]], file_namer(d))
          if (file.exists(out_file) && !overwrite) stop("File exists: ", out_file)
          writeLines(shard_lines, out_file)
          out_paths[[d]] <- out_file
        }
      }
      invisible(out_paths)
    }
    
    close_logger <- function() close(con)
    
    list(
      path = normalizePath(log_path, mustWork = FALSE),
      write = write_line,
      split = split_into_sublogs,
      close = close_logger
    )
  }
  
  ## return created logger
  create_logger(log_path, append, with_timestamp, enforce_code)
}

# ✅ Usage:
logger <- setup_logging()

logger$write("01234 Action started")
logger$close()
