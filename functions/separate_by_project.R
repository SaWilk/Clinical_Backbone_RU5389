# -------------------------------------------------------------------------
# separate_by_project()
#
# Purpose:
#   Splits a dataset into separate data frames by project identifier and
#   saves them to disk. Handles both numeric project IDs and labels that
#   embed the project number (e.g. "P 123 ...").
#
# Behavior:
#   - Detects the project column, searching in this order of preference:
#       • "project"
#       • "Projekt...Project"
#       • "Projekt."
#       • "Projekt"
#       • "projekt"
#       • "p"
#       • "proj"
#       • "Proj"
#   - Cleans project column values (trims whitespace, removes test markers).
#   - Infers the sample name from:
#       • The `sample` argument if supplied.
#       • Otherwise from the variable name of the input data frame.
#       • Recognizes "adults", "adolescents", "children_parents"/"children".
#       • Falls back to "unknown" if not found.
#   - Project label parsing:
#       • "P <digits>" at the start → treated as "general_info" project.
#       • Pure digits (e.g. "123") → treated as numeric project ID.
#       • Any string with digits → digits are extracted as the project ID.
#       • Other cases → project ID set to NA ("unknown").
#   - Output objects:
#       • If "general_info": creates `data_general_info_<sample>`.
#       • Otherwise: creates `data_<sample>_p_<pid>`.
#   - Output files:
#       • For "general_info": saves as
#         `general_info_<date>_<sample>.xlsx` (+ CSV if requested).
#       • For numeric projects: saves as `<pid>_<date>_<sample>.xlsx`
#         (+ CSV if requested), skipping IDs 0 and 99 (kept in memory only).
#   - Files are saved under `out_path`; directories are created as needed.
#
# Input:
#   df         : data frame to split by project.
#   out_path   : directory path where files are saved (required if no
#                global `out_path` exists).
#   sample     : optional character string ("adults", "adolescents",
#                "children_parents"); overrides automatic detection.
#   export_csv : logical, if TRUE also export CSV alongside XLSX.
#
# Output:
#   - Returns TRUE (invisibly).
#   - Creates project-specific data frames in the global environment.
#   - Writes XLSX (and optional CSV) files to disk.
#
# Example:
#   separate_by_project(dat_adults, out_path = "exports", export_csv = TRUE)
#
# -------------------------------------------------------------------------
