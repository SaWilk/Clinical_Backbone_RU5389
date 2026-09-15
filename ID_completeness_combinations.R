# =============================================================================
# ID completeness: logical conversion and modality-combination counts
# =============================================================================

# ---- Configuration ----------------------------------------------------------

input_file <- paste0(
  "K:/Wilken/_Arbeitsordner/Backbone/_Analysis/_RU5389/_private/",
  "ID Completeness Across RU.xlsx"
)

output_dir <- "K:/Wilken/_Arbeitsordner/Backbone/_Analysis/_RU5389/out"

# Empty cells and explicit false values in TASK, ECG, PUPIL, QUEST, and
# COG-TEST are interpreted as FALSE. Every other non-empty value is counted as
# TRUE, so unusual entries do not stop the analysis.

# Install missing CRAN packages automatically.
install_missing_packages <- TRUE

# ---- Packages ---------------------------------------------------------------

required_packages <- c("readxl", "writexl", "ggplot2")
optional_packages <- c("ggVennDiagram", "ComplexUpset")

ensure_packages <- function(packages, required = TRUE) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE) && install_missing_packages) {
      message("Installing package: ", package)
      try(
        install.packages(package, repos = "https://cloud.r-project.org"),
        silent = TRUE
      )
    }

    if (!requireNamespace(package, quietly = TRUE) && required) {
      stop(
        "Required R package '", package, "' is unavailable. Install it with ",
        "install.packages(\"", package, "\") and run the script again.",
        call. = FALSE
      )
    }
  }
}

ensure_packages(required_packages, required = TRUE)
ensure_packages(optional_packages, required = FALSE)

# ---- Helper functions -------------------------------------------------------

normalize_column_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  toupper(gsub("[^A-Z0-9]", "", x))
}

is_blank_value <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

convert_to_logical_safely <- function(x, column_label) {
  source_class <- paste(class(x), collapse = "/")
  original_missing <- sum(is_blank_value(x))

  normalized <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  normalized <- toupper(trimws(normalized))
  normalized[normalized %in% c("", "NA", "N/A", "NULL")] <- NA_character_

  true_values <- c("WAHR", "TRUE", "T", "JA", "YES", "Y", "1", "X")
  false_values <- c("FALSCH", "FALSE", "F", "NEIN", "NO", "N", "0")

  other_nonempty <- !is.na(normalized) &
    !normalized %in% c(true_values, false_values)

  # Missing/blank and explicitly false values become FALSE. Recognized true
  # values and every other non-empty entry become TRUE.
  result <- !is.na(normalized) & !normalized %in% false_values

  list(
    value = as.logical(result),
    qc = data.frame(
      Column = column_label,
      Source_class = source_class,
      Missing_or_blank_treated_as_FALSE = original_missing,
      Other_nonempty_treated_as_TRUE = sum(other_nonempty),
      TRUE_after_conversion = sum(result),
      FALSE_after_conversion = sum(!result),
      stringsAsFactors = FALSE
    )
  )
}

make_exact_label <- function(pattern_row, modality_labels) {
  present <- modality_labels[as.logical(pattern_row)]

  if (length(present) == 0L) {
    return("No listed data type")
  }
  if (length(present) == 1L) {
    return(paste("Only", present))
  }
  if (length(present) < length(modality_labels)) {
    return(paste0(paste(present, collapse = " + "), " (exact)"))
  }
  paste(present, collapse = " + ")
}

make_boolean_key <- function(x) {
  apply(x, 1L, function(row) paste(as.integer(as.logical(row)), collapse = ""))
}

# ---- Read the relevant worksheet -------------------------------------------

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file, call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

required_normalized_names <- c("ID", "TASK", "ECG", "PUPIL", "QUEST", "COGTEST")
sheet_names <- readxl::excel_sheets(input_file)

sheet_headers <- lapply(sheet_names, function(sheet_name) {
  names(readxl::read_excel(
    input_file,
    sheet = sheet_name,
    n_max = 0,
    .name_repair = "unique"
  ))
})

eligible_sheets <- vapply(sheet_headers, function(header) {
  all(required_normalized_names %in% normalize_column_name(header))
}, logical(1))

if (!any(eligible_sheets)) {
  stop(
    "No worksheet contains all required columns: ID, TASK, ECG, PUPIL, ",
    "QUEST, and COG-TEST.",
    call. = FALSE
  )
}

selected_sheet <- sheet_names[which(eligible_sheets)[1L]]

if (sum(eligible_sheets) > 1L) {
  message(
    "Several worksheets contain the required columns. Using the first one: ",
    selected_sheet
  )
}

raw_data <- readxl::read_excel(
  input_file,
  sheet = selected_sheet,
  .name_repair = "unique"
)

# Remove only rows that are completely empty across all imported columns.
completely_empty <- apply(raw_data, 1L, function(row) all(is_blank_value(row)))
data <- raw_data[!completely_empty, , drop = FALSE]

if (nrow(data) == 0L) {
  stop("The selected worksheet contains no participant rows.", call. = FALSE)
}

normalized_names <- normalize_column_name(names(data))

find_column <- function(target) {
  matches <- which(normalized_names == target)
  if (length(matches) != 1L) {
    stop(
      "Expected exactly one column matching '", target, "', but found ",
      length(matches), ".",
      call. = FALSE
    )
  }
  names(data)[matches]
}

id_column <- find_column("ID")
modality_columns <- c(
  TASK = find_column("TASK"),
  ECG = find_column("ECG"),
  PUPIL = find_column("PUPIL"),
  QUEST = find_column("QUEST"),
  `COG-TEST` = find_column("COGTEST")
)

# ---- Convert availability columns to logical -------------------------------

conversion_qc <- vector("list", length(modality_columns))

for (i in seq_along(modality_columns)) {
  modality_label <- names(modality_columns)[i]
  source_column <- unname(modality_columns[i])

  converted <- convert_to_logical_safely(
    data[[source_column]],
    column_label = modality_label
  )

  data[[source_column]] <- converted$value
  converted$qc$Source_column_name <- source_column
  conversion_qc[[i]] <- converted$qc
}

conversion_qc <- do.call(rbind, conversion_qc)
conversion_qc <- conversion_qc[, c(
  "Column", "Source_column_name", "Source_class",
  "Missing_or_blank_treated_as_FALSE", "Other_nonempty_treated_as_TRUE",
  "TRUE_after_conversion", "FALSE_after_conversion"
)]

# ---- Prepare row identifiers ------------------------------------------------
# Every non-empty table row is counted. Duplicate or missing IDs are retained.

id_key <- trimws(as.character(data[[id_column]]))
id_missing <- is_blank_value(data[[id_column]])
nonmissing_ids <- id_key[!id_missing]
duplicate_id_rows <- sum(duplicated(nonmissing_ids))
row_key <- sprintf("row_%07d", seq_len(nrow(data)))

# Use consistent, human-readable modality names for all analyses.
availability <- as.data.frame(
  lapply(modality_columns, function(column_name) data[[column_name]]),
  check.names = FALSE
)
names(availability) <- names(modality_columns)
modality_labels <- names(availability)

# ---- Count all 2^5 mutually exclusive combinations -------------------------

all_patterns <- expand.grid(
  rep(list(c(FALSE, TRUE)), length(modality_labels)),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
names(all_patterns) <- modality_labels

all_pattern_keys <- make_boolean_key(all_patterns)
observed_keys <- make_boolean_key(availability)
observed_counts <- table(factor(observed_keys, levels = all_pattern_keys))

exact_counts <- all_patterns
exact_counts$Number_of_data_types <- rowSums(all_patterns)
exact_counts$Combination <- vapply(
  seq_len(nrow(all_patterns)),
  function(i) make_exact_label(all_patterns[i, ], modality_labels),
  character(1)
)
exact_counts$Count <- as.integer(observed_counts)
exact_counts$Percent <- if (nrow(data) == 0L) {
  0
} else {
  100 * exact_counts$Count / nrow(data)
}

exact_counts <- exact_counts[, c(
  "Combination", "Number_of_data_types", modality_labels, "Count", "Percent"
)]
exact_counts <- exact_counts[
  order(-exact_counts$Count, -exact_counts$Number_of_data_types,
        exact_counts$Combination),
]
rownames(exact_counts) <- NULL

# ---- Count all inclusive intersections -------------------------------------
# Example: TASK + ECG counts everyone with both, even if they also have PUPIL.

subsets <- unlist(
  lapply(seq_along(modality_labels), function(k) {
    combn(modality_labels, k, simplify = FALSE)
  }),
  recursive = FALSE
)

inclusive_counts <- do.call(rbind, lapply(subsets, function(required_modalities) {
  has_all <- Reduce(`&`, availability[required_modalities])
  data.frame(
    Required_modalities = paste(required_modalities, collapse = " + "),
    Number_of_required_data_types = length(required_modalities),
    Additional_data_types_allowed = TRUE,
    Count = sum(has_all),
    Percent = if (nrow(data) == 0L) 0 else 100 * sum(has_all) / nrow(data),
    stringsAsFactors = FALSE
  )
}))

inclusive_counts <- inclusive_counts[
  order(inclusive_counts$Number_of_required_data_types,
        -inclusive_counts$Count,
        inclusive_counts$Required_modalities),
]
rownames(inclusive_counts) <- NULL

# ---- Per-modality totals ----------------------------------------------------

modality_totals <- data.frame(
  Data_type = modality_labels,
  Available_TRUE = vapply(availability, sum, integer(1)),
  Unavailable_FALSE = vapply(availability, function(x) sum(!x), integer(1)),
  stringsAsFactors = FALSE
)
modality_totals$Percent_available <- if (nrow(data) == 0L) {
  0
} else {
  100 * modality_totals$Available_TRUE / nrow(data)
}

analysis_metadata <- data.frame(
  Item = c(
    "Input file", "Worksheet", "Participants counted",
    "Completely empty rows removed", "Rows with missing ID retained",
    "Duplicate-ID rows retained", "Logical conversion rule",
    "Exact-count check"
  ),
  Value = c(
    input_file,
    selected_sheet,
    as.character(nrow(data)),
    as.character(sum(completely_empty)),
    as.character(sum(id_missing)),
    as.character(duplicate_id_rows),
    "Blank/explicit false = FALSE; every other non-empty value = TRUE",
    paste0(sum(exact_counts$Count), " = ", nrow(data))
  ),
  stringsAsFactors = FALSE
)

if (sum(exact_counts$Count) != nrow(data)) {
  stop("Internal count check failed: exact combination counts do not sum to N.")
}

# ---- Save Excel outputs -----------------------------------------------------

cleaned_file <- file.path(output_dir, "ID_Completeness_Across_RU_logical.xlsx")
counts_file <- file.path(output_dir, "ID_Completeness_combination_counts.xlsx")

writexl::write_xlsx(
  list("Cleaned data" = data),
  path = cleaned_file
)

writexl::write_xlsx(
  list(
    "Exact combinations" = exact_counts,
    "Inclusive overlaps" = inclusive_counts,
    "Modality totals" = modality_totals,
    "Conversion QC" = conversion_qc,
    "Analysis metadata" = analysis_metadata
  ),
  path = counts_file
)

# ---- Plot exact combinations as a readable bar chart -----------------------

plot_counts <- exact_counts[exact_counts$Count > 0L, , drop = FALSE]
plot_counts$Combination <- factor(
  plot_counts$Combination,
  levels = rev(plot_counts$Combination[order(plot_counts$Count)])
)

combination_plot <- ggplot2::ggplot(
  plot_counts,
  ggplot2::aes(x = Combination, y = Count)
) +
  ggplot2::geom_col(fill = "#2C7FB8", width = 0.75) +
  ggplot2::geom_text(
    ggplot2::aes(label = Count),
    hjust = -0.15,
    size = 4
  ) +
  ggplot2::coord_flip(clip = "off") +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.12))
  ) +
  ggplot2::labs(
    title = "Exact combinations of available data types",
    subtitle = "Each participant contributes to exactly one bar",
    x = NULL,
    y = "Number of participants"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    plot.title.position = "plot",
    plot.margin = ggplot2::margin(10, 28, 10, 10)
  )

ggplot2::ggsave(
  filename = file.path(output_dir, "ID_Completeness_exact_combinations.png"),
  plot = combination_plot,
  width = 12,
  height = max(7, 0.38 * nrow(plot_counts) + 2.5),
  units = "in",
  dpi = 300,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(output_dir, "ID_Completeness_exact_combinations.pdf"),
  plot = combination_plot,
  width = 12,
  height = max(7, 0.38 * nrow(plot_counts) + 2.5),
  units = "in",
  bg = "white"
)

# ---- Optional five-set Venn diagram ----------------------------------------

venn_created <- FALSE

if (requireNamespace("ggVennDiagram", quietly = TRUE)) {
  tryCatch({
    venn_sets <- setNames(
      lapply(modality_labels, function(modality) row_key[availability[[modality]]]),
      modality_labels
    )

    venn_plot <- ggVennDiagram::ggVennDiagram(
      venn_sets,
      label_alpha = 0,
      label = "count",
      edge_size = 0.7,
      set_size = 4
    ) +
      ggplot2::scale_fill_gradient(low = "#F7FBFF", high = "#2171B5") +
      ggplot2::labs(
        title = "Overlap of available data types",
        subtitle = "Five-set Venn diagram; use the Excel table for exact counts",
        fill = "Count"
      ) +
      ggplot2::theme(
        plot.title.position = "plot",
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12)
      )

    ggplot2::ggsave(
      filename = file.path(output_dir, "ID_Completeness_venn.png"),
      plot = venn_plot,
      width = 13,
      height = 10,
      units = "in",
      dpi = 300,
      bg = "white"
    )

    ggplot2::ggsave(
      filename = file.path(output_dir, "ID_Completeness_venn.pdf"),
      plot = venn_plot,
      width = 13,
      height = 10,
      units = "in",
      bg = "white"
    )

    venn_created <- TRUE
  }, error = function(error) {
    message(
      "The Venn diagram could not be created: ", conditionMessage(error),
      ". All tables and the exact-combination bar chart were still created."
    )
  })
} else {
  message(
    "Package 'ggVennDiagram' is unavailable. Tables and the exact-combination ",
    "bar chart were still created."
  )
}

# ---- Optional UpSet plot ----------------------------------------------------

upset_created <- FALSE

if (requireNamespace("ComplexUpset", quietly = TRUE)) {
  tryCatch({
    upset_data <- cbind(data.frame(Row = row_key), availability)

    upset_plot <- ComplexUpset::upset(
      upset_data,
      intersect = modality_labels,
      name = "Data type",
      min_size = 1,
      width_ratio = 0.25,
      base_annotations = list(
        "Exact combination size" =
          ComplexUpset::intersection_size(text = list(size = 4)) +
          ggplot2::ylab("Participants")
      )
    ) +
      ggplot2::labs(
        title = "Exact overlaps of available data types",
        subtitle = "UpSet view of observed combinations"
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12)
      )

    ggplot2::ggsave(
      filename = file.path(output_dir, "ID_Completeness_upset.png"),
      plot = upset_plot,
      width = 14,
      height = 8,
      units = "in",
      dpi = 300,
      bg = "white"
    )

    ggplot2::ggsave(
      filename = file.path(output_dir, "ID_Completeness_upset.pdf"),
      plot = upset_plot,
      width = 14,
      height = 8,
      units = "in",
      bg = "white"
    )

    upset_created <- TRUE
  }, error = function(error) {
    message(
      "The UpSet plot could not be created: ", conditionMessage(error),
      ". All tables and the exact-combination bar chart were still created."
    )
  })
} else {
  message(
    "Package 'ComplexUpset' is unavailable. Tables, the exact-combination bar ",
    "chart, and (if available) the Venn diagram were still created."
  )
}

# ---- Completion message -----------------------------------------------------

message("Analysis complete. Participants counted: ", nrow(data))
message("Cleaned logical data: ", cleaned_file)
message("Combination counts: ", counts_file)
message("Exact-combination plot: created")
message("Venn diagram: ", if (venn_created) "created" else "not created")
message("UpSet plot: ", if (upset_created) "created" else "not created")
