# ---- Setup ----
auto_install <- FALSE
pkgs <- c("readxl", "dplyr", "stringr", "tidyr", "ggplot2", "tools", "forcats")
if (auto_install) {
  to_get <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
  if (length(to_get)) install.packages(to_get)
}
lapply(pkgs, require, character.only = TRUE)

# ---- Configurable inputs ----
data_dir   <- "01_project_data"        # subfolder with .xlsx files
out_dir    <- "out"                    # subfolder for output
output_png <- file.path(out_dir, "project_progress.png")
png_width  <- 10
png_height <- 5
png_dpi    <- 300

# Colors and styles
fill_color    <- "#C8B79E"  # darker beige/bone
outline_color <- "black"
refline_color <- "grey60"

# Manual axis labels (edit freely)
axis_labels <- c(p2="p2", p3="p3", p4="p4", p5="p5", p6="p6", p7="p7", p8="p8", p9="p9")

# Desired sample sizes
total_targets <- c(
  p2 = 80,
  p3 = 130,
  p4 = NA,   # not yet determined
  p5 = 100,
  p6 = 80,
  p7 = 400,
  p8 = 194,
  p9 = 120
)

# ---- Helpers ----
`%||%` <- function(a, b) if (is.null(a)) b else a

count_observations <- function(path) {
  df <- suppressMessages(readxl::read_excel(path))
  if (!is.data.frame(df) || nrow(df) == 0) return(0L)
  df_clean <- dplyr::filter(df, !dplyr::if_all(dplyr::everything(), ~ is.na(.x)))
  nrow(df_clean)
}

parse_filename <- function(f) {
  base <- tools::file_path_sans_ext(basename(f))
  parts <- strsplit(base, "_", fixed = TRUE)[[1]]
  if (length(parts) < 3) {
    return(data.frame(project=NA_integer_, date=NA_character_, sample=NA_character_))
  }
  data.frame(
    project = suppressWarnings(as.integer(parts[1])),
    date    = parts[2],
    sample  = tolower(paste(parts[3:length(parts)], collapse = "_")),
    stringsAsFactors = FALSE
  )
}

# ---- Aggregation rules ----
# p6: NO EXCEPTION (use default sum of rows)
# p7: adolescents + adults
# p8: adults + children + floor(children_parents_rows / 3)
tally_rules <- function(df) {
  df$sample <- tolower(df$sample %||% "")
  df %>%
    dplyr::group_by(project) %>%
    dplyr::summarise(
      n_default    = sum(n, na.rm = TRUE),
      n_p7_combo   = sum(dplyr::if_else(sample %in% c("adolescents", "adults"), n, 0L), na.rm = TRUE),
      n_p8_basic   = sum(dplyr::if_else(sample %in% c("children", "adults"), n, 0L), na.rm = TRUE),
      n_p8_cp_rows = sum(dplyr::if_else(sample == "children_parents", n, 0L), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      n_p8_combo = n_p8_basic + floor(n_p8_cp_rows / 3),
      n_final = dplyr::case_when(
        project == 7 ~ n_p7_combo,    # adolescents + adults
        project == 8 ~ n_p8_combo,    # adults + children + floor(children_parents/3)
        TRUE ~ n_default              # everyone else, incl. p6: plain count
      )
    ) %>%
    dplyr::select(project, n = n_final)
}

# ---- Load & count ----
files <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE)

meta <- if (length(files)) do.call(rbind, lapply(files, parse_filename)) else
  data.frame(project=integer(), date=character(), sample=character(), stringsAsFactors=FALSE)
meta$path <- files
meta <- dplyr::filter(meta, !is.na(project), project >= 2, project <= 9)
meta$n <- if (nrow(meta)) vapply(meta$path, count_observations, integer(1)) else integer(0)

agg <- tally_rules(meta)

# Impute zeros for missing projects
agg_full <- dplyr::left_join(data.frame(project = 2:9), agg, by = "project") %>%
  dplyr::mutate(n = tidyr::replace_na(n, 0L),
                project_id = paste0("p", project))

# Attach total_n and compute %
progress <- agg_full %>%
  dplyr::mutate(total_n = as.numeric(total_targets[project_id]),
                pct = ifelse(is.na(total_n) | total_n == 0, NA_real_, 100 * n / total_n),
                pct_clamped = pmin(pct, 100))

# ---- Plot ----
progress$project_id <- factor(progress$project_id,
                              levels = paste0("p", 2:9),
                              labels = axis_labels[paste0("p", 2:9)])

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

p <- ggplot(progress, aes(x = forcats::fct_inorder(project_id))) +
  # 100% outlines (black)
  geom_col(aes(y = 100), fill = NA, color = outline_color, linewidth = 0.9, width = 0.7) +
  # filled progress
  geom_col(aes(y = pct_clamped), fill = fill_color, width = 0.7) +
  # vertical reference lines at 25/50/75 (drawn as hlines before coord_flip)
  geom_hline(yintercept = c(25, 50, 75), linetype = "dashed", linewidth = 0.3, color = refline_color) +
  # label just beyond the filled bar (n / total and %)
  geom_text(
    aes(y = pmin(pct_clamped + 3, 100),
        label = ifelse(is.na(total_n),
                       sprintf("%d (target: TBD)", n),
                       sprintf("%d / %d (%.0f%%)", n, total_n, pct))),
    hjust = 0,
    size = 3.6
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(limits = c(0, 110), breaks = c(0, 25, 50, 75, 100), expand = expansion(mult = c(0.02, 0.12))) +
  labs(x = NULL, y = "Percent of target (0–100%)", title = "Recruitment Progress by Project") +
  theme_classic(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.title.y     = element_blank()
  )

ggsave(output_png, p, width = png_width, height = png_height, dpi = png_dpi)
message("Saved: ", normalizePath(output_png))

# ---- Optional: print a compact table ----
progress %>%
  dplyr::mutate(pct = ifelse(is.na(pct), NA, round(pct, 1))) %>%
  dplyr::select(project_id, n, total_n, pct) %>%
  print(n = Inf)
