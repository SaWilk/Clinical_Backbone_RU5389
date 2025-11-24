# quick “Find in Files” UI: Ctrl+Shift+F (Win) / Cmd+Shift+F (Mac)

# or programmatic search from the repo root:
patterns <- c("old-data","all_backbone_data")
files <- list.files(".", recursive = TRUE, full.names = TRUE,
                    ignore.case = TRUE, include.dirs = FALSE)
files <- files[!grepl("^\\.git", files)]  # skip .git

hits <- lapply(files, function(f){
  ext <- tools::file_ext(f)
  if (ext %in% c("R","Rmd","r","Rprofile","Renviron","yml","yaml","json","txt","md","sh","ps1","bat","cmd")) {
    ln <- try(readLines(f, warn = FALSE, encoding = "UTF-8"), silent = TRUE)
    if (inherits(ln, "try-error")) return(NULL)
    where <- which(Reduce(`|`, lapply(patterns, grepl, x = ln, fixed = TRUE)))
    if (length(where)) data.frame(file=f, line=where, text=ln[where], check.names=FALSE)
  }
})
do.call(rbind, Filter(NROW, hits))
