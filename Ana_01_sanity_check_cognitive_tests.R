library(tidyverse)
library(readxl)
library(patchwork)

# -----------------------------------------------------------------------------
# 1. Directory and File Setup
# -----------------------------------------------------------------------------
base_dir  <- "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389"
data_dir  <- file.path(base_dir, "01_project_data", "all_projects_backbone", "derivatives", "experiment_data")
output_dir <- file.path(base_dir, "plots_sanity_check")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# File patterns (including typo fallback for 'sores' vs 'scores')
patterns <- c(
  "*_adults_cognitive_sores\\.xlsx$",
  "*_adults_cognitive_scores\\.xlsx$",
  "*_children_parents_cognitive_scores\\.xlsx$",
  "*_adolescents_cognitive_scores\\.xlsx$"
)

file_list <- list.files(
  path = data_dir, 
  pattern = paste(patterns, collapse = "|"), 
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(file_list) == 0) {
  stop("No matching cognitive score Excel files found in target directory.")
}

# -----------------------------------------------------------------------------
# 2. Data Loading & Cleaning
# -----------------------------------------------------------------------------
raw_df <- file_list %>%
  map_dfr(~ read_excel(.x, col_types = "text") %>% mutate(source_file = basename(.x)))

# Clean numeric formats (e.g., German decimal commas "2118,837" -> "2118.837")
cleaned_df <- raw_df %>%
  mutate(across(everything(), ~ str_replace_all(.x, ",", "."))) %>%
  mutate(across(everything(), ~ case_when(
    .x %in% c("WAHR", "TRUE", "true") ~ "TRUE",
    .x %in% c("FALSCH", "FALSE", "false") ~ "FALSE",
    TRUE ~ .x
  )))

# Identify metadata ID columns to exclude from automated univariate plotting
id_cols <- c("vp_id", "source_file", "TIME", "total", "samplescore_vp_id")
target_cols <- setdiff(names(cleaned_df), id_cols)

# Ensure project column 'p' is present
if (!"p" %in% names(cleaned_df)) {
  stop("Column 'p' (project) not found in dataset. Check column naming.")
}

# Auto-convert numeric columns
df <- cleaned_df %>%
  mutate(across(all_of(target_cols), ~ {
    num_val <- suppressWarnings(as.numeric(.x))
    # Keep numeric if majority of non-NA values convert cleanly
    if (sum(!is.na(num_val)) > 0.5 * sum(!is.na(.x))) num_val else as.character(.x)
  })) %>%
  mutate(p = factor(p))

# -----------------------------------------------------------------------------
# 3. Plotting Functions
# -----------------------------------------------------------------------------
theme_set(theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold")))

# Plot builder for numeric columns
plot_numeric <- function(data, var_name) {
  p_overall <- ggplot(data, aes(x = .data[[var_name]])) +
    geom_histogram(fill = "#2b5c8f", color = "white", bins = 30, alpha = 0.8) +
    geom_rug(alpha = 0.5) +
    labs(title = paste("Overall:", var_name), x = var_name, y = "Count")
  
  p_split <- ggplot(data, aes(x = p, y = .data[[var_name]], fill = p)) +
    geom_boxplot(alpha = 0.6, outlier.colour = "red", outlier.size = 2) +
    geom_jitter(width = 0.15, alpha = 0.3, size = 1) +
    labs(title = paste("By Project (p):", var_name), x = "Project (p)", y = var_name) +
    theme(legend.position = "none")
  
  p_overall / p_split
}

# Plot builder for categorical / boolean columns
plot_categorical <- function(data, var_name) {
  p_overall <- ggplot(data, aes(x = factor(.data[[var_name]]))) +
    geom_bar(fill = "#388e3c", alpha = 0.8) +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3, size = 3.5) +
    labs(title = paste("Overall:", var_name), x = var_name, y = "Count")
  
  p_split <- ggplot(data, aes(x = p, fill = factor(.data[[var_name]]))) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_brewer(palette = "Set2", name = var_name) +
    labs(title = paste("Proportion by Project (p):", var_name), x = "Project (p)", y = "Percentage") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  p_overall / p_split
}

# -----------------------------------------------------------------------------
# 4. Generate & Export Plots
# -----------------------------------------------------------------------------
pdf_path <- file.path(output_dir, "cognitive_data_sanity_check.pdf")
pdf(pdf_path, width = 10, height = 8)

feature_cols <- setdiff(target_cols, "p")

for (col in feature_cols) {
  # Skip columns with no variation or all NA
  if (all(is.na(df[[col]])) || length(unique(na.omit(df[[col]]))) <= 1) next
  
  if (is.numeric(df[[col]])) {
    plt <- plot_numeric(df, col)
  } else {
    plt <- plot_categorical(df, col)
  }
  
  # Print to PDF page
  print(plt)
  
  # Save individual PNG
  ggsave(
    filename = file.path(output_dir, paste0("check_", col, ".png")),
    plot = plt,
    width = 10,
    height = 8,
    dpi = 300
  )
}

dev.off()

message("Sanity check complete. PDF report saved to: ", pdf_path)