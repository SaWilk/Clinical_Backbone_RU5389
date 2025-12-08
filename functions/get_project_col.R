get_project_col <- function(df) {
  nms <- names(df)
  low <- tolower(nms)
  cand <- c("project","p","proj","project_id","projectid")
  hit <- which(low %in% cand)
  if (length(hit)) return(nms[hit[1]])
  NULL
}
