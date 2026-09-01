# Vergleicht die in Excel referenzierten Testdateien mit den tatsächlich
# vorhandenen Dateien. Es werden nur Abweichungen ausgegeben.

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Bitte zuerst 'readxl' installieren: install.packages('readxl')")
}

backbone_dir <- "K:/Wilken_Arbeitsordner/Clinical_Backbone_RU5389/01_project_data/all_projects_backbone"
experiment_dir <- file.path(backbone_dir, "raw_data", "experiment_data")
cogtest_dir <- file.path(experiment_dir, "ALL_2026-08-27_adults_cogtest_data")
output_dir <- file.path(
  backbone_dir, "derivatives", "experiment_data",
  "cogtest_filename_audit_2026-08-27"
)
tests <- c("BACS", "WCST", "LNS")

if (!dir.exists(cogtest_dir)) stop("Datenordner nicht gefunden: ", cogtest_dir)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Passende Exceldatei und Tabellenblatt finden.
xlsx_files <- list.files(
  experiment_dir,
  pattern = "^ALL_2026-08-27_adults_cogtests?\\.xlsx$",
  full.names = TRUE,
  ignore.case = TRUE
)
if (length(xlsx_files) != 1L) {
  stop(
    "Es wurde nicht genau eine passende Exceldatei gefunden. Gefunden: ",
    if (length(xlsx_files)) paste(basename(xlsx_files), collapse = ", ") else "keine"
  )
}
xlsx_file <- xlsx_files[[1]]

sheet_name <- NULL
for (s in readxl::excel_sheets(xlsx_file)) {
  headers <- tolower(trimws(names(readxl::read_excel(xlsx_file, sheet = s, n_max = 0))))
  if (all(c("participant", "id", tolower(tests)) %in% headers)) {
    sheet_name <- s
    break
  }
}
if (is.null(sheet_name)) {
  stop("Kein Tabellenblatt mit participant, id, BACS, WCST und LNS gefunden.")
}

overview <- readxl::read_excel(xlsx_file, sheet = sheet_name)
names(overview) <- trimws(names(overview))

find_col <- function(x) {
  hit <- names(overview)[tolower(names(overview)) == tolower(x)]
  if (length(hit) != 1L) stop("Spalte fehlt oder ist nicht eindeutig: ", x)
  hit[[1]]
}

participant_col <- find_col("participant")
id_col <- find_col("id")
test_cols <- setNames(vapply(tests, find_col, character(1)), tests)

basename_any <- function(x) {
  sub("^.*[\\\\/]", "", trimws(as.character(x)))
}

extract_uuid <- function(x) {
  pattern <- "[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{12}"
  vapply(as.character(x), function(z) {
    if (is.na(z) || !nzchar(z)) return(NA_character_)
    m <- regexpr(pattern, z, perl = TRUE)
    if (m[[1]] < 0L) NA_character_ else tolower(regmatches(z, m))
  }, character(1))
}

clean_id <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  sub("\\.0$", "", x)
}

collapse_values <- function(x) {
  x <- unique(x[!is.na(x) & nzchar(x)])
  if (length(x)) paste(x, collapse = " | ") else NA_character_
}

# UUID -> numerische ID aus participant und id.
participant_map <- data.frame(
  participant_uuid = extract_uuid(overview[[participant_col]]),
  id = clean_id(overview[[id_col]]),
  stringsAsFactors = FALSE
)
participant_map <- participant_map[
  !is.na(participant_map$participant_uuid) & !is.na(participant_map$id), , drop = FALSE
]

# Erwartete Dateinamen aus den drei Excelspalten.
expected <- do.call(rbind, lapply(tests, function(test) {
  values <- as.character(overview[[test_cols[[test]]]])
  keep <- !is.na(values) & nzchar(trimws(values))
  data.frame(
    test = test,
    excel_row = which(keep) + 1L,
    id = clean_id(overview[[id_col]][keep]),
    participant_uuid = extract_uuid(overview[[participant_col]][keep]),
    expected_filename = basename_any(values[keep]),
    stringsAsFactors = FALSE
  )
}))
row.names(expected) <- NULL
expected$filename_uuid <- extract_uuid(expected$expected_filename)
expected$file_key <- tolower(trimws(expected$expected_filename))

# Tatsächlich vorhandene Dateien im Ordner.
actual_paths <- list.files(
  cogtest_dir,
  pattern = "(BACS|WCST|LNS).*\\.txt$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)
if (!length(actual_paths)) {
  stop("Keine BACS-, WCST- oder LNS-TXT-Dateien im Datenordner gefunden.")
}

actual <- data.frame(
  folder_filename = basename(actual_paths),
  folder_path = normalizePath(actual_paths, winslash = "/", mustWork = FALSE),
  stringsAsFactors = FALSE
)
actual$test <- ifelse(
  grepl("BACS", actual$folder_filename, ignore.case = TRUE), "BACS",
  ifelse(
    grepl("WCST", actual$folder_filename, ignore.case = TRUE), "WCST",
    ifelse(grepl("LNS", actual$folder_filename, ignore.case = TRUE), "LNS", NA_character_)
  )
)
actual$filename_uuid <- extract_uuid(actual$folder_filename)
actual$file_key <- tolower(trimws(actual$folder_filename))

# 1a: In Excel referenziert, aber nicht als Datei vorhanden.
excel_without_file <- expected[
  !(expected$file_key %in% actual$file_key),
  c("test", "excel_row", "id", "participant_uuid", "expected_filename", "filename_uuid"),
  drop = FALSE
]
names(excel_without_file)[names(excel_without_file) == "filename_uuid"] <- "expected_filename_uuid"

# 1b: Im Ordner vorhanden, aber in Excel nirgends referenziert.
folder_without_excel <- actual[
  !(actual$file_key %in% expected$file_key),
  c("test", "folder_filename", "filename_uuid", "folder_path"),
  drop = FALSE
]

# Für jede nicht referenzierte Ordnerdatei die ID über UUID -> participant bestimmen.
id_lookup <- lapply(folder_without_excel$filename_uuid, function(u) {
  ids <- if (is.na(u)) character() else unique(participant_map$id[participant_map$participant_uuid == u])
  ids <- ids[!is.na(ids)]
  list(
    inferred_id = if (length(ids) == 1L) ids[[1]] else NA_character_,
    id_candidates = collapse_values(ids),
    id_mapping_status = if (!length(ids)) {
      "UUID nicht in participant-Spalte gefunden"
    } else if (length(ids) == 1L) {
      "ID eindeutig über UUID zugeordnet"
    } else {
      "UUID ist mehreren IDs zugeordnet"
    }
  )
})
folder_without_excel$inferred_id <- vapply(id_lookup, `[[`, character(1), "inferred_id")
folder_without_excel$id_candidates <- vapply(id_lookup, `[[`, character(1), "id_candidates")
folder_without_excel$id_mapping_status <- vapply(id_lookup, `[[`, character(1), "id_mapping_status")

# 2: Nur die beiden Differenzmengen über Test + numerische ID zusammenführen.
valid_excel <- !is.na(excel_without_file$id)
valid_folder <- !is.na(folder_without_excel$inferred_id)
matching_keys <- unique(rbind(
  data.frame(
    test = excel_without_file$test[valid_excel],
    id = excel_without_file$id[valid_excel],
    stringsAsFactors = FALSE
  ),
  data.frame(
    test = folder_without_excel$test[valid_folder],
    id = folder_without_excel$inferred_id[valid_folder],
    stringsAsFactors = FALSE
  )
))

id_matching <- do.call(rbind, lapply(seq_len(nrow(matching_keys)), function(i) {
  test <- matching_keys$test[i]
  id <- matching_keys$id[i]
  excel_hit <- excel_without_file$test == test & excel_without_file$id == id
  folder_hit <- folder_without_excel$test == test & folder_without_excel$inferred_id == id
  n_excel <- sum(excel_hit, na.rm = TRUE)
  n_folder <- sum(folder_hit, na.rm = TRUE)

  assessment <- if (n_excel == 1L && n_folder == 1L) {
    "Eindeutiger 1:1-Kandidat über ID"
  } else if (n_excel > 0L && n_folder > 0L) {
    paste0("Mehrdeutig: ", n_excel, " Excelname(n), ", n_folder, " Ordnerdatei(en)")
  } else if (n_excel > 0L) {
    "Kein nicht-referenzierter Ordnerkandidat mit gleicher ID"
  } else {
    "Keine fehlende Excelreferenz mit gleicher ID"
  }

  data.frame(
    test = test,
    id = id,
    n_excel_without_file = n_excel,
    excel_filenames = collapse_values(excel_without_file$expected_filename[excel_hit]),
    n_folder_without_excel = n_folder,
    folder_filenames = collapse_values(folder_without_excel$folder_filename[folder_hit]),
    assessment = assessment,
    stringsAsFactors = FALSE
  )
}))

if (is.null(id_matching)) {
  id_matching <- data.frame(
    test = character(), id = character(), n_excel_without_file = integer(),
    excel_filenames = character(), n_folder_without_excel = integer(),
    folder_filenames = character(), assessment = character()
  )
}

# Fälle ohne ermittelbare ID bleiben ebenfalls in der Matchingübersicht sichtbar.
excel_no_id <- excel_without_file[is.na(excel_without_file$id), , drop = FALSE]
folder_no_id <- folder_without_excel[is.na(folder_without_excel$inferred_id), , drop = FALSE]

if (nrow(excel_no_id)) {
  id_matching <- rbind(id_matching, data.frame(
    test = excel_no_id$test,
    id = NA_character_,
    n_excel_without_file = 1L,
    excel_filenames = excel_no_id$expected_filename,
    n_folder_without_excel = 0L,
    folder_filenames = NA_character_,
    assessment = "Excelreferenz ohne verwertbare numerische ID",
    stringsAsFactors = FALSE
  ))
}
if (nrow(folder_no_id)) {
  id_matching <- rbind(id_matching, data.frame(
    test = folder_no_id$test,
    id = NA_character_,
    n_excel_without_file = 0L,
    excel_filenames = NA_character_,
    n_folder_without_excel = 1L,
    folder_filenames = folder_no_id$folder_filename,
    assessment = folder_no_id$id_mapping_status,
    stringsAsFactors = FALSE
  ))
}

id_matching <- id_matching[order(id_matching$test, id_matching$id, na.last = TRUE), , drop = FALSE]
row.names(id_matching) <- NULL

summary_table <- do.call(rbind, lapply(tests, function(test) {
  data.frame(
    test = test,
    filenames_in_excel_without_file = sum(excel_without_file$test == test),
    files_in_folder_without_excel = sum(folder_without_excel$test == test),
    unique_one_to_one_id_matches = sum(
      id_matching$test == test & id_matching$assessment == "Eindeutiger 1:1-Kandidat über ID"
    ),
    stringsAsFactors = FALSE
  )
}))

tables <- list(
  Kurzuebersicht = summary_table,
  Excel_ohne_Datei = excel_without_file,
  Ordner_ohne_Excel = folder_without_excel,
  Matching_nach_ID = id_matching
)
xlsx_output <- file.path(output_dir, "cogtest_filename_differences_adults_2026-08-27.xlsx")

if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(tables, xlsx_output)
  message("Ergebnis gespeichert: ", xlsx_output)
} else if (requireNamespace("openxlsx", quietly = TRUE)) {
  openxlsx::write.xlsx(tables, xlsx_output, overwrite = TRUE)
  message("Ergebnis gespeichert: ", xlsx_output)
} else {
  write.csv2(summary_table, file.path(output_dir, "01_Kurzuebersicht.csv"), row.names = FALSE, fileEncoding = "UTF-8-BOM")
  write.csv2(excel_without_file, file.path(output_dir, "02_Excel_ohne_Datei.csv"), row.names = FALSE, fileEncoding = "UTF-8-BOM")
  write.csv2(folder_without_excel, file.path(output_dir, "03_Ordner_ohne_Excel.csv"), row.names = FALSE, fileEncoding = "UTF-8-BOM")
  write.csv2(id_matching, file.path(output_dir, "04_Matching_nach_ID.csv"), row.names = FALSE, fileEncoding = "UTF-8-BOM")
  message("Kein Excel-Schreibpaket gefunden; vier CSV-Dateien gespeichert in: ", output_dir)
}

print(summary_table, row.names = FALSE)
