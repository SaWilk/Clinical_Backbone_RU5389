# ============================================================
# Exploratory Factor Analysis (EFA) Pipeline (config-driven)
# - Works at item-level (polychoric), subscale-level (Pearson),
#   and (theoretical) diagnosis-level (tetrachoric).
# - Splits into training/test, chooses #factors (default scree+CNG),
#   fits EFA, optionally evaluates replication in test set,
#   and writes outputs + plots to ROOT/out/factor_analysis/efa
# ============================================================

# ---------------------------
# 0) CONFIG (human-switchable)
# ---------------------------
config <- list(
  
  # Project root:
  # - If NULL, the script tries to auto-detect the R project root (.Rproj, git, here()).
  # - Set manually if you prefer.
  root = NULL,
  
  # Analysis level:
  # - "items": ordinal questionnaire items (default). Usually use polychoric correlations.
  # - "subscales": continuous/approximately continuous scores. Usually use Pearson correlations.
  # - "diagnoses": (theoretical) binary indicators. Would use tetrachoric correlations.
  analysis_level = "items",  # c("items","subscales","diagnoses")
  
  # Input files (relative to ROOT/03_analysis_input):
  input = list(
    dir_rel = "03_analysis_input",
    file_items = "adults_HiTOP_items.xlsx",
    file_subscales = "adults_HiTOP_subscales.xlsx"
    # diagnoses not implemented (no file yet)
  ),
  
  # Optional stratification info (e.g., project variable) produced separately:
  stratification = list(
    use = TRUE,
    # Default location you described:
    path_rel = file.path("03_analysis_input", "analysis_ready", "stratification_info.xlsx"),
    
    # How to merge into the analysis data:
    # - "auto": if a shared key column exists, join by it; else if same nrow, bind by row order.
    # - "by_key": join by `key` (must exist in both).
    # - "by_row_order": cbind by row order (requires same nrow).
    merge_method = "auto",  # c("auto","by_key","by_row_order")
    key = NULL,             # e.g., "participant_id" if you have it later
    
    # Candidate stratification variable name (must be in stratification_info after merge)
    strata_var = "project"
  ),
  
  # Training/test split:
  split = list(
    do_split = TRUE,
    seed = 20251216,
    prop = 0.75,           # proportion assigned to training
    strata_var = "project" # set NULL to disable stratification
  ),
  
  # Correlation matrix choice:
  # - "auto": based on analysis_level (items->polychoric; subscales->Pearson; diagnoses->tetrachoric)
  # - you can force a specific one if needed.
  correlation = list(
    type = "auto",       # c("auto","polychoric","pearson","tetrachoric")
    use_pairwise = TRUE, # for Pearson (pairwise.complete.obs)
    smooth = TRUE,       # for poly/tetra: smooth correlation matrix if needed
    mc_cores = 2         # poly/tetra can be sped up by multiple cores (psych honors options(mc.cores))
  ),
  
  # Adequacy checks (prints + can halt):
  adequacy = list(
    # KMO conventions (very rough): <.50 unacceptable; .50-.59 miserable; .60-.69 mediocre; .70-.79 middling; .80-.89 meritorious; >=.90 marvelous
    kmo_min = 0.60,
    # Bartlett test should be significant (p < alpha) to suggest correlations differ from identity
    bartlett_alpha = 0.05,
    
    # If checks fail:
    # - If interactive: pause and ask whether to continue
    # - If non-interactive: stop
    halt_on_fail = TRUE
  ),
  
  # Sample size / stability heuristics (warnings, not hard rules)
  stability = list(
    # For EFA, low N/p ratios tend to be unstable unless communalities are high and the structure is strong.
    min_n = 200,
    min_n_per_var = 5, # e.g., N / p < 5 triggers a warning
    warn_only = TRUE
  ),
  
  
  # Variable selection:
  # - By default, the pipeline uses all numeric-ish manifest columns, excluding obvious IDs/time columns
  #   and excluding the stratification column used for splitting (if any).
  # - If you want explicit control, set include_names (exact column names) or include_regex.
  variables = list(
    include_names = NULL,    # e.g., c("ASRS[001]","ASRS[002]", ...)
    include_regex = NULL,    # e.g., "^ASRS\\["
    exclude_names = NULL,    # e.g., c("project","site")
    exclude_regex = NULL     # e.g., "^(id|project)$"
  ),
  
  # Factor retention (how many factors k?):
  retention = list(
    # Default: scree plot + CNG index suggestion, then you choose k.
    # Alternative: parallel analysis.
    method = "scree_cng",     # c("scree_cng","parallel")
    prompt_for_k = TRUE,      # if FALSE, uses k_fixed
    k_fixed = NULL,           # set to integer to bypass prompting
    parallel = list(
      n_iter = 50,            # replications; larger = more stable but slower
      fa = "fa"               # "fa" vs "pc" (we keep FA)
    )
  ),
  
  # EFA estimator, rotation, and scores:
  efa = list(
    # Engine:
    # - "psych": psych::fa (fast, flexible; common in psychometrics).
    # - "lavaan": lavaan EFA syntax; supports WLSMV naturally for ordered items (but may be heavier).
    engine = "psych",               # c("psych","lavaan")
    
    # psych::fa options:
    # fm (factor extraction):
    # - "ml": maximum likelihood (enables some fit indices; assumes multivariate normality for ML theory; often used even with polychorics as a pragmatic choice)
    # - "minres": minimum residual (robust-ish, often good default when ML assumptions are dubious)
    # - "pa": principal axis factoring (classic; less distributional assumptions than ML)
    fm = "ml",
    
    # rotation:
    # - oblique rotations ("oblimin","promax","geominQ") allow correlated factors (often realistic in psychopathology)
    # - orthogonal rotations (e.g., "varimax") force uncorrelated factors (often unrealistic here)
    rotate = "oblimin",             # c("oblimin","promax","geominQ","targetQ")
    
    # Factor scores:
    # - "tenBerge": correl-preserving for oblique/orthogonal solutions (often a good general choice)
    # - "Thurstone"/"regression": maximizes correlation with factors; can distort factor correlations
    # - "Bartlett": unbiased under model assumptions; often good when uniquenesses are reliable
    # - "Anderson": orthogonal-only, yields uncorrelated scores
    # - "none": skip scoring
    scores_method = "tenBerge",     # c("tenBerge","Thurstone","regression","Bartlett","Anderson","none")
    
    # Missing data handling for scoring:
    scores_impute = "median",       # c("none","mean","median")
    
    # For lavaan engine:
    lavaan = list(
      estimator = "WLSMV",          # common for ordered items
      rotation = "geomin"           # lavaan EFA rotation keyword
    )
  ),
  
  # Hierarchy options:
  hierarchy = list(
    # - "none": plain EFA only
    # - "bass_ackwards": exploratory hierarchical decomposition (useful for finding p-factor / higher-order structure)
    # - "latent_model": placeholder (would move into CFA/SEM territory)
    method = "none",                # c("none","bass_ackwards","latent_model")
    bass_ackwards = list(
      max_factors = 6               # how far to decompose (2..max)
    )
  ),
  
  # Test-sample validation:
  validation = list(
    refit_on_test = TRUE,           # fit same k on test, compare loadings
    congruence = TRUE,              # compute Tucker congruence between train/test loadings
    score_correlations = TRUE       # correlate factor scores between train/test solutions (informal)
  ),
  
  # Reliability report (per factor, based on assigned items):
  reliability = list(
    loading_cutoff = 0.30,          # include items with |loading| >= cutoff
    crossloading_cutoff = 0.20,     # exclude items whose 2nd-highest |loading| >= crossloading_cutoff
    min_items_per_factor = 3,       # fewer items -> reliability estimates become noisy/unhelpful
    compute_alpha = TRUE,           # Cronbach alpha (quick; assumes tau-equivalence)
    compute_omega = TRUE            # McDonald's omega (less strict than alpha; still model-dependent)
  ),
  
  # Output locations:
  output = list(
    dir_rel = file.path("out", "factor_analysis", "efa"),
    plots_subdir = "plots"
  )
)


# ---------------------------
# 1) Packages + utilities
# ---------------------------
required_pkgs <- c(
  "readxl", "writexl", "dplyr", "tibble", "rsample",
  "psych", "ggplot2"
)

optional_pkgs <- c(
  "here", "rprojroot", "nFactors", "corrplot", "stringr"
)

check_packages <- function(pkgs, optional = FALSE) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    msg <- paste0(
      if (optional) "Optional" else "Required",
      " packages missing: ", paste(missing, collapse = ", "),
      "\nInstall with: install.packages(c(", paste(sprintf('"%s"', missing), collapse = ", "), "))"
    )
    if (optional) {
      message(msg)
    } else {
      stop(msg, call. = FALSE)
    }
  }
}

check_packages(required_pkgs, optional = FALSE)
check_packages(optional_pkgs, optional = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(writexl)
  library(dplyr)
  library(tibble)
  library(rsample)
  library(psych)
  library(ggplot2)
})

safe_mkdir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

get_project_root <- function() {
  # Try `here` first
  if (requireNamespace("here", quietly = TRUE)) {
    root <- tryCatch(here::here(), error = function(e) NULL)
    if (!is.null(root) && dir.exists(root)) return(normalizePath(root, winslash = "/"))
  }
  # Try rprojroot (.Rproj)
  if (requireNamespace("rprojroot", quietly = TRUE)) {
    root <- tryCatch(rprojroot::find_root(rprojroot::is_rstudio_project), error = function(e) NULL)
    if (!is.null(root) && dir.exists(root)) return(normalizePath(root, winslash = "/"))
    # Try git root as fallback
    root <- tryCatch(rprojroot::find_root(rprojroot::is_git_root), error = function(e) NULL)
    if (!is.null(root) && dir.exists(root)) return(normalizePath(root, winslash = "/"))
  }
  # Fallback
  normalizePath(getwd(), winslash = "/")
}

pause_or_stop <- function(msg, halt = TRUE) {
  warning(msg, call. = FALSE)
  if (!halt) return(invisible(TRUE))
  
  if (interactive()) {
    ans <- readline("Adequacy gate failed. Type 'YES' to continue anyway, anything else to stop: ")
    if (toupper(trimws(ans)) == "YES") return(invisible(TRUE))
    stop("Stopped due to adequacy gate failure.", call. = FALSE)
  } else {
    stop("Stopped due to adequacy gate failure (non-interactive run).", call. = FALSE)
  }
}

# Brief helper: save a ggplot (PNG)
save_plot_png <- function(p, filename, width = 9, height = 6, dpi = 200) {
  ggsave(filename = filename, plot = p, width = width, height = height, dpi = dpi)
}

# ---------------------------
# 2) Resolve paths + read data
# ---------------------------
if (is.null(config$root)) {
  config$root <- get_project_root()
} else {
  config$root <- normalizePath(config$root, winslash = "/", mustWork = FALSE)
}

message("Project ROOT = ", config$root)

input_dir <- file.path(config$root, config$input$dir_rel)
out_dir   <- file.path(config$root, config$output$dir_rel)
plots_dir <- file.path(out_dir, config$output$plots_subdir)

safe_mkdir(out_dir)
safe_mkdir(plots_dir)

# Save a config snapshot for reproducibility
writeLines(capture.output(str(config)), file.path(out_dir, "config_snapshot.txt"))

# Choose input file based on analysis level
get_input_path <- function() {
  if (config$analysis_level == "items") {
    return(file.path(input_dir, config$input$file_items))
  } else if (config$analysis_level == "subscales") {
    return(file.path(input_dir, config$input$file_subscales))
  } else if (config$analysis_level == "diagnoses") {
    stop("Diagnosis-level branch is not implemented because no diagnoses input file is defined yet.", call. = FALSE)
  } else {
    stop("Unknown analysis_level: ", config$analysis_level, call. = FALSE)
  }
}

data_path <- get_input_path()
if (!file.exists(data_path)) stop("Input file not found: ", data_path, call. = FALSE)

df <- readxl::read_excel(data_path) |> as.data.frame()

message("Loaded data: ", basename(data_path), " | N = ", nrow(df), " | p = ", ncol(df))

# ---------------------------
# 2b) Merge in stratification_info.xlsx (optional)
# ---------------------------
merge_stratification <- function(df) {
  if (!isTRUE(config$stratification$use)) return(df)
  
  strat_path <- file.path(config$root, config$stratification$path_rel)
  if (!file.exists(strat_path)) {
    message("Stratification file not found (continuing without): ", strat_path)
    return(df)
  }
  
  strat <- readxl::read_excel(strat_path) |> as.data.frame()
  message("Loaded stratification info: ", basename(strat_path), " | N = ", nrow(strat), " | p = ", ncol(strat))
  
  method <- config$stratification$merge_method
  
  if (method == "by_key") {
    key <- config$stratification$key
    if (is.null(key)) stop("merge_method='by_key' but config$stratification$key is NULL", call. = FALSE)
    if (!(key %in% names(df)) || !(key %in% names(strat))) stop("Key column not found in both df and strat: ", key, call. = FALSE)
    out <- dplyr::left_join(df, strat, by = key)
    return(out)
  }
  
  if (method == "by_row_order") {
    if (nrow(df) != nrow(strat)) stop("Row-order merge requested but nrow(df) != nrow(strat).", call. = FALSE)
    # Avoid duplicate column names
    dup <- intersect(names(df), names(strat))
    if (length(dup) > 0) {
      strat <- strat |> dplyr::select(-dplyr::all_of(dup))
    }
    return(bind_cols(df, strat))
  }
  
  # auto:
  # 1) try an obvious join key
  key_candidates <- intersect(names(df), names(strat))
  if (!is.null(config$stratification$key) && config$stratification$key %in% key_candidates) {
    key <- config$stratification$key
    out <- dplyr::left_join(df, strat, by = key)
    return(out)
  }
  
  # 2) try common "id" patterns
  id_like <- key_candidates[grepl("(^id$|participant|subject|case)", key_candidates, ignore.case = TRUE)]
  if (length(id_like) >= 1) {
    key <- id_like[1]
    message("Auto-merge: joining by key = ", key)
    out <- dplyr::left_join(df, strat, by = key)
    return(out)
  }
  
  # 3) fallback: row-order bind if same n
  if (nrow(df) == nrow(strat)) {
    message("Auto-merge: no shared key found; binding stratification info by row order (same N).")
    dup <- intersect(names(df), names(strat))
    if (length(dup) > 0) strat <- strat |> dplyr::select(-dplyr::all_of(dup))
    return(bind_cols(df, strat))
  }
  
  message("Auto-merge failed (no shared key and different N). Continuing without stratification info.")
  df
}

df <- merge_stratification(df)

# ---------------------------
# 3) Split into training/test
# ---------------------------
if (isTRUE(config$split$do_split)) {
  set.seed(config$split$seed)
  
  strata_var <- config$split$strata_var
  if (!is.null(strata_var) && !(strata_var %in% names(df))) {
    message("Requested stratification variable '", strata_var, "' not found. Disabling stratification.")
    strata_var <- NULL
  }
  
  if (is.null(strata_var)) {
    sp <- rsample::initial_split(df, prop = config$split$prop)
  } else {
    sp <- rsample::initial_split(df, prop = config$split$prop, strata = all_of(strata_var))
  }
  
  df_training <- rsample::training(sp)
  df_test     <- rsample::testing(sp)
  
} else {
  df_training <- df
  df_test <- NULL
}

message("Training N = ", nrow(df_training), if (!is.null(df_test)) paste0(" | Test N = ", nrow(df_test)) else "")

# ---------------------------
# 4) Prepare analysis variables
# ---------------------------
# For now, we treat ALL columns as manifest variables *except* obvious non-numeric columns.
# If you later add IDs, timestamps, etc., add them to `non_manifest_patterns`.
non_manifest_patterns <- c("^id$", "timestamp", "date", "time")

is_manifest <- function(x, nm) {
  if (is.character(x) || is.logical(x)) return(FALSE)
  if (any(grepl(paste(non_manifest_patterns, collapse = "|"), nm, ignore.case = TRUE))) return(FALSE)
  TRUE
}

select_manifest <- function(df_in) {
  # Start from columns that look like manifest variables
  keep <- mapply(function(col, nm) is_manifest(col, nm), df_in, names(df_in))
  
  X <- df_in[, keep, drop = FALSE]
  
  # Exclude stratification-related columns (even if numeric-coded)
  auto_exclude <- unique(na.omit(c(
    config$split$strata_var,
    config$stratification$strata_var,
    config$stratification$key
  )))
  if (length(auto_exclude) > 0) {
    X <- X[, setdiff(names(X), auto_exclude), drop = FALSE]
  }
  
  # User-specified include/exclude
  if (!is.null(config$variables$include_names)) {
    X <- X[, intersect(names(X), config$variables$include_names), drop = FALSE]
  }
  if (!is.null(config$variables$include_regex)) {
    X <- X[, grepl(config$variables$include_regex, names(X)), drop = FALSE]
  }
  if (!is.null(config$variables$exclude_names)) {
    X <- X[, setdiff(names(X), config$variables$exclude_names), drop = FALSE]
  }
  if (!is.null(config$variables$exclude_regex)) {
    X <- X[, !grepl(config$variables$exclude_regex, names(X)), drop = FALSE]
  }
  
  X
}

X_train <- select_manifest(df_training)
X_test  <- if (!is.null(df_test)) select_manifest(df_test) else NULL

# Sample size stability warning
N <- nrow(X_train)
p <- ncol(X_train)
ratio <- N / p

if (N < config$stability$min_n || ratio < config$stability$min_n_per_var) {
  warning(
    sprintf(
      "Potential instability warning: N=%d, p=%d, N/p=%.2f. Low N relative to p can yield unstable EFA solutions. Consider subscales or stronger regularization/constraints.",
      N, p, ratio
    ),
    call. = FALSE
  )
}

# ---------------------------
# 5) Correlation matrix (R)
# ---------------------------
options(mc.cores = config$correlation$mc_cores)

infer_corr_type <- function() {
  if (config$correlation$type != "auto") return(config$correlation$type)
  
  if (config$analysis_level == "items") return("polychoric")
  if (config$analysis_level == "subscales") return("pearson")
  if (config$analysis_level == "diagnoses") return("tetrachoric")
  stop("Cannot infer correlation type.")
}

corr_type <- infer_corr_type()
message("Correlation type = ", corr_type)

prep_for_ordinal_corr <- function(X) {
  # Convert each column to ordered factor if it looks categorical/ordinal.
  # We do this only for the correlation computation; we keep numeric X for scoring/reliability.
  X2 <- X
  for (nm in names(X2)) {
    v <- X2[[nm]]
    if (is.factor(v)) {
      X2[[nm]] <- as.ordered(v)
      next
    }
    # Convert numeric to ordered factor using observed levels
    if (is.numeric(v)) {
      u <- unique(v[!is.na(v)])
      # Heuristic: if integer-ish with limited categories, treat as ordinal
      if (length(u) <= 10 && all(abs(u - round(u)) < 1e-8)) {
        X2[[nm]] <- as.ordered(factor(v, levels = sort(unique(v), na.last = NA)))
      } else {
        # leave as numeric; polychoric will still work but might not be meaningful if truly continuous
        X2[[nm]] <- v
      }
    }
  }
  X2
}

compute_R <- function(X) {
  if (corr_type == "pearson") {
    use <- if (isTRUE(config$correlation$use_pairwise)) "pairwise.complete.obs" else "complete.obs"
    R <- suppressWarnings(stats::cor(X, use = use))
    return(R)
  }
  
  if (corr_type == "polychoric") {
    Xo <- prep_for_ordinal_corr(X)
    pc <- suppressWarnings(psych::polychoric(Xo, correct = 0, smooth = config$correlation$smooth))
    return(pc$rho)
  }
  
  if (corr_type == "tetrachoric") {
    Xo <- prep_for_ordinal_corr(X)
    tc <- suppressWarnings(psych::tetrachoric(Xo, correct = 0, smooth = config$correlation$smooth))
    return(tc$rho)
  }
  
  stop("Unknown correlation type: ", corr_type, call. = FALSE)
}

R_train <- compute_R(X_train)
write.csv(R_train, file.path(out_dir, "correlation_matrix_training.csv"), row.names = TRUE)

# Plot: correlation heatmap (may be heavy for very large p; skip if p>250)
if (p <= 250) {
  # Use ggplot heatmap (no extra deps)
  Rm <- as.data.frame(as.table(R_train))
  names(Rm) <- c("Var1", "Var2", "r")
  
  heat <- ggplot(Rm, aes(x = Var1, y = Var2, fill = r)) +
    geom_tile() +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title  = element_blank()
    ) +
    labs(title = "Correlation matrix (training)")
  
  save_plot_png(heat, file.path(plots_dir, "correlation_heatmap_training.png"), width = 9, height = 8)
} else {
  message("Skipping correlation heatmap plot because p > 250.")
}

# ---------------------------
# 6) Adequacy checks (KMO + Bartlett)
# ---------------------------
N_eff <- nrow(X_train)

kmo <- psych::KMO(R_train)
bart <- psych::cortest.bartlett(R_train, n = N_eff)

message("\n--- Adequacy checks (training) ---")
message("KMO (overall MSA): ", round(kmo$MSA, 3))
message("Bartlett test: chi2 = ", round(bart$chisq, 2), ", df = ", bart$df, ", p = ", signif(bart$p.value, 3))

# Gatekeeping
fails <- c()
if (is.finite(kmo$MSA) && kmo$MSA < config$adequacy$kmo_min) {
  fails <- c(fails, sprintf("KMO %.3f < %.2f", kmo$MSA, config$adequacy$kmo_min))
}
if (is.finite(bart$p.value) && bart$p.value > config$adequacy$bartlett_alpha) {
  fails <- c(fails, sprintf("Bartlett p=%.3g > %.2f", bart$p.value, config$adequacy$bartlett_alpha))
}

if (length(fails) > 0) {
  pause_or_stop(
    paste("Adequacy checks out of range:", paste(fails, collapse = "; ")),
    halt = isTRUE(config$adequacy$halt_on_fail)
  )
}

# ---------------------------
# 7) Choose number of factors (k)
# ---------------------------
eigs <- eigen(R_train, only.values = TRUE)$values
scree_df <- data.frame(
  k = seq_along(eigs),
  eigenvalue = eigs
)

k_suggest_cng <- NA_integer_
if (requireNamespace("nFactors", quietly = TRUE)) {
  # CNG procedure
  cng <- tryCatch(nFactors::nCng(eigs, details = TRUE), error = function(e) NULL)
  if (!is.null(cng)) k_suggest_cng <- as.integer(cng$nFactors)
} else {
  message("Package nFactors not installed: CNG suggestion will be skipped.")
}

scree_plot <- ggplot(scree_df, aes(x = k, y = eigenvalue)) +
  geom_line() +
  geom_point() +
  theme_minimal(base_size = 11) +
  labs(title = "Scree plot (training)", x = "Component / factor number", y = "Eigenvalue")

if (is.finite(k_suggest_cng)) {
  scree_plot <- scree_plot +
    geom_vline(xintercept = k_suggest_cng, linetype = "dashed") +
    annotate("text", x = k_suggest_cng, y = max(eigs, na.rm = TRUE), label = paste0("CNG suggests k=", k_suggest_cng),
             vjust = -0.5, hjust = 0.0)
}

save_plot_png(scree_plot, file.path(plots_dir, "scree_plot_cng_training.png"))

# Parallel analysis option
k_suggest_parallel <- NA_integer_
if (config$retention$method == "parallel") {
  message("\nRunning parallel analysis (training)...")
  cor_arg <- if (corr_type %in% c("polychoric","tetrachoric")) "poly" else "cor"
  
  pa <- psych::fa.parallel(
    X_train,
    fm = config$efa$fm,
    fa = config$retention$parallel$fa,
    n.iter = config$retention$parallel$n_iter,
    cor = cor_arg,
    plot = TRUE
  )
  
  # fa.parallel returns suggested nfactors in $nfact for fa, and $ncomp for pc (depending on fa argument)
  k_suggest_parallel <- tryCatch({
    if (!is.null(pa$nfact)) as.integer(pa$nfact) else as.integer(pa$nfactors)
  }, error = function(e) NA_integer_)
  
  # Save the last plot (base graphics)
  png(file.path(plots_dir, "parallel_analysis_training.png"), width = 1200, height = 800, res = 160)
  psych::fa.parallel(
    X_train,
    fm = config$efa$fm,
    fa = config$retention$parallel$fa,
    n.iter = config$retention$parallel$n_iter,
    cor = cor_arg,
    plot = TRUE
  )
  dev.off()
}

choose_k <- function() {
  # If fixed, use it
  if (!is.null(config$retention$k_fixed)) return(as.integer(config$retention$k_fixed))
  
  # If not prompting, use suggestion
  if (!isTRUE(config$retention$prompt_for_k)) {
    if (config$retention$method == "parallel" && is.finite(k_suggest_parallel)) return(k_suggest_parallel)
    if (is.finite(k_suggest_cng)) return(k_suggest_cng)
    # fallback heuristic: Kaiser > 1
    return(sum(eigs > 1, na.rm = TRUE))
  }
  
  # Prompt user
  default_k <- if (config$retention$method == "parallel" && is.finite(k_suggest_parallel)) k_suggest_parallel else k_suggest_cng
  if (!is.finite(default_k)) default_k <- max(1, sum(eigs > 1, na.rm = TRUE))
  
  message("\nScree plot saved to: ", file.path(plots_dir, "scree_plot_cng_training.png"))
  if (config$retention$method == "parallel") {
    message("Parallel analysis plot saved to: ", file.path(plots_dir, "parallel_analysis_training.png"))
  }
  ans <- readline(paste0("How many factors (k) do you want to extract? [default ", default_k, "]: "))
  if (nchar(trimws(ans)) == 0) return(as.integer(default_k))
  as.integer(ans)
}

k <- choose_k()
if (is.na(k) || k < 1) stop("Invalid k selected: ", k, call. = FALSE)

message("Chosen number of factors k = ", k)

# ---------------------------
# 8) Fit EFA on training set
# ---------------------------
fit_efa_psych <- function(R, n_obs) {
  # Note: psych::fa can also be called on raw data and set cor="poly"/"tet".
  # We use the correlation matrix directly to keep the pipeline explicit.
  psych::fa(
    r = R,
    nfactors = k,
    fm = config$efa$fm,
    rotate = config$efa$rotate,
    scores = if (config$efa$scores_method == "none") "none" else config$efa$scores_method,
    n.obs = n_obs
  )
}

fit_efa_lavaan <- function(X) {
  # Lavaan EFA syntax: efa("block")*F1 =~ x1 + x2 + ...
  # This is optional and may be heavy for large p.
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("lavaan not installed; install.packages('lavaan') or set config$efa$engine='psych'", call. = FALSE)
  }
  
  vars <- names(X)
  # Build efa block with k factors
  rhs <- paste(vars, collapse = " + ")
  lines <- character()
  for (i in seq_len(k)) {
    lines <- c(lines, paste0('efa("block1")*F', i, ' =~ ', rhs))
  }
  model <- paste(lines, collapse = "\n")
  
  ordered_vars <- NULL
  if (corr_type %in% c("polychoric","tetrachoric")) ordered_vars <- vars
  
  lavaan::cfa(
    model = model,
    data = X,
    estimator = config$efa$lavaan$estimator,
    ordered = ordered_vars,
    rotation = config$efa$lavaan$rotation,
    std.lv = TRUE
  )
}

efa_train <- if (config$efa$engine == "psych") {
  fit_efa_psych(R_train, n_obs = nrow(X_train))
} else {
  fit_efa_lavaan(X_train)
}

# Save the fitted object
saveRDS(efa_train, file.path(out_dir, "efa_training_fit.rds"))

# ---------------------------
# 9) Extract + export training results
# ---------------------------
extract_loadings_psych <- function(fa_obj) {
  L <- as.data.frame(unclass(fa_obj$loadings))
  L <- tibble::rownames_to_column(L, "variable")
  L
}

extract_fit_psych <- function(fa_obj) {
  tibble::tibble(
    k = fa_obj$factors,
    fm = fa_obj$fm,
    rotate = fa_obj$rotate,
    RMSR = unname(fa_obj$rms),
    RMSEA = unname(fa_obj$RMSEA[1]),
    RMSEA_lower = unname(fa_obj$RMSEA[2]),
    RMSEA_upper = unname(fa_obj$RMSEA[3]),
    TLI = unname(fa_obj$TLI),
    BIC = unname(fa_obj$BIC)
  )
}

if (config$efa$engine == "psych") {
  loadings_train <- extract_loadings_psych(efa_train)
  fit_train <- extract_fit_psych(efa_train)
  
  # Communalities & uniquenesses
  comm <- tibble::tibble(variable = names(efa_train$communality), communality = unname(efa_train$communality))
  uniq <- tibble::tibble(variable = names(efa_train$uniquenesses), uniqueness = unname(efa_train$uniquenesses))
  
  # Factor correlations (Phi) if oblique
  Phi <- if (!is.null(efa_train$Phi)) as.data.frame(efa_train$Phi) else NULL
  
  # Save to xlsx
  sheets <- list(
    fit = fit_train,
    loadings = loadings_train,
    communalities = comm,
    uniquenesses = uniq
  )
  if (!is.null(Phi)) sheets$factor_correlations <- tibble::rownames_to_column(Phi, "factor")
  
  writexl::write_xlsx(sheets, file.path(out_dir, "efa_training_solution.xlsx"))
  
  # Plot: factor diagram (can be busy with many variables)
  png(file.path(plots_dir, "factor_diagram_training.png"), width = 1400, height = 1000, res = 160)
  try(psych::fa.diagram(efa_train, simple = FALSE, main = "EFA factor diagram (training)"), silent = TRUE)
  dev.off()
  
} else {
  # Lavaan export: keep minimal in this pipeline
  sink(file.path(out_dir, "efa_training_solution_lavaan_summary.txt"))
  print(lavaan::summary(efa_train, fit.measures = TRUE, standardized = TRUE))
  sink()
}

# ---------------------------
# 10) Factor scores (training + test) using training solution
# ---------------------------
compute_scores_psych <- function(fa_obj, X) {
  if (config$efa$scores_method == "none") return(NULL)
  # factor.scores expects x = data, f = loadings matrix (pattern/structure handled internally)
  # Use missing=TRUE so it will impute if requested.
  sc <- psych::factor.scores(
    x = X,
    f = fa_obj,
    method = config$efa$scores_method,
    missing = TRUE,
    impute = config$efa$scores_impute
  )
  as.data.frame(sc$scores)
}

scores_train <- NULL
scores_test  <- NULL
if (config$efa$engine == "psych") {
  scores_train <- compute_scores_psych(efa_train, X_train)
  if (!is.null(scores_train)) {
    write.csv(scores_train, file.path(out_dir, "factor_scores_training.csv"), row.names = FALSE)
  }
  if (!is.null(X_test)) {
    scores_test <- compute_scores_psych(efa_train, X_test)
    if (!is.null(scores_test)) {
      write.csv(scores_test, file.path(out_dir, "factor_scores_test_using_training_solution.csv"), row.names = FALSE)
    }
  }
}

# ---------------------------
# 11) Hierarchy option (Bass-Ackwards)
# ---------------------------
if (config$hierarchy$method == "bass_ackwards") {
  if (config$efa$engine != "psych") {
    message("Bass-Ackwards is implemented here only for psych::fa objects; skipping.")
  } else {
    max_f <- config$hierarchy$bass_ackwards$max_factors
    message("Running bassAckward decomposition up to ", max_f, " factors (training)...")
    
    ba <- tryCatch(
      psych::bassAckward(
        r = R_train,
        nfactors = max_f,
        fm = config$efa$fm,
        rotate = config$efa$rotate,
        n.obs = nrow(X_train)
      ),
      error = function(e) NULL
    )
    
    if (!is.null(ba)) {
      saveRDS(ba, file.path(out_dir, "bassAckward_training.rds"))
      sink(file.path(out_dir, "bassAckward_training_summary.txt"))
      print(ba)
      sink()
    } else {
      message("bassAckward failed (possibly due to correlation/estimation issues).")
    }
  }
}

# ---------------------------
# 12) Validation on test sample (refit + congruence)
# ---------------------------
validation_outputs <- list()

if (!is.null(X_test) && isTRUE(config$validation$refit_on_test) && config$efa$engine == "psych") {
  message("\n--- Validation: refit EFA on test sample ---")
  
  R_test <- compute_R(X_test)
  write.csv(R_test, file.path(out_dir, "correlation_matrix_test.csv"), row.names = TRUE)
  
  efa_test <- tryCatch(
    psych::fa(
      r = R_test,
      nfactors = k,
      fm = config$efa$fm,
      rotate = config$efa$rotate,
      scores = "none",
      n.obs = nrow(X_test)
    ),
    error = function(e) NULL
  )
  
  if (!is.null(efa_test)) {
    saveRDS(efa_test, file.path(out_dir, "efa_test_fit_refit.rds"))
    
    loadings_test <- extract_loadings_psych(efa_test)
    writexl::write_xlsx(list(loadings = loadings_test), file.path(out_dir, "efa_test_solution_refit.xlsx"))
    
    # Congruence between training and test loadings (Tucker congruence)
    if (isTRUE(config$validation$congruence)) {
      L_train <- as.matrix(unclass(efa_train$loadings))
      L_test  <- as.matrix(unclass(efa_test$loadings))
      
      cong <- psych::factor.congruence(L_train, L_test)
      write.csv(cong, file.path(out_dir, "tucker_congruence_train_vs_test.csv"), row.names = TRUE)
      validation_outputs$congruence <- cong
      
      # Quick, human-friendly summary: for each training factor, best-matching test factor congruence
      best_match <- apply(cong, 1, function(r) max(r, na.rm = TRUE))
      best_idx   <- apply(cong, 1, function(r) which.max(r))
      cong_summary <- data.frame(
        train_factor = rownames(cong),
        best_test_factor = colnames(cong)[best_idx],
        congruence = as.numeric(best_match),
        row.names = NULL
      )
      write.csv(cong_summary, file.path(out_dir, "tucker_congruence_summary_best_matches.csv"), row.names = FALSE)
      
      message("Saved congruence matrix to: ", file.path(out_dir, "tucker_congruence_train_vs_test.csv"))
      message("Best-match congruence summary (train -> best test factor):")
      print(cong_summary)
    }
  } else {
    message("Test refit failed; skipping congruence.")
  }
}

# ---------------------------
# 13) Reliability reporting (train + test)
# ---------------------------
# We create "factor scales" based on the *training* loadings:
# - include items with a strong primary loading and limited cross-loading.
# This is just one reasonable choice; you can change rules in config$reliability.
assign_items_to_factors <- function(loadings_mat) {
  lc <- config$reliability$loading_cutoff
  cc <- config$reliability$crossloading_cutoff
  
  absL <- abs(loadings_mat)
  primary_factor <- apply(absL, 1, which.max)
  primary_loading <- apply(absL, 1, max)
  
  second_loading <- apply(absL, 1, function(x) sort(x, decreasing = TRUE)[2])
  
  keep <- (primary_loading >= lc) & (second_loading < cc)
  
  data.frame(
    variable = rownames(loadings_mat),
    primary_factor = primary_factor,
    primary_loading = primary_loading,
    second_loading = second_loading,
    keep = keep
  )
}

compute_reliability_for_factor <- function(X, vars) {
  # Returns a named list of reliability estimates for a set of variables.
  out <- list(n_items = length(vars))
  if (length(vars) < config$reliability$min_items_per_factor) return(out)
  
  Xi <- X[, vars, drop = FALSE]
  
  if (isTRUE(config$reliability$compute_alpha)) {
    a <- tryCatch(psych::alpha(Xi, check.keys = TRUE, warnings = FALSE), error = function(e) NULL)
    if (!is.null(a)) {
      out$alpha_raw <- unname(a$total$raw_alpha)
      out$alpha_std <- unname(a$total$std.alpha)
    }
  }
  
  if (isTRUE(config$reliability$compute_omega)) {
    # omega can be slow; do 1-factor omega for the items assigned to each factor
    o <- tryCatch(psych::omega(Xi, nfactors = 1, plot = FALSE, warnings = FALSE), error = function(e) NULL)
    if (!is.null(o)) {
      # omega.tot is typical for a unidimensional set; omega.h is relevant when a general factor is estimated
      out$omega_total <- unname(o$omega.tot)
      out$omega_h     <- unname(o$omega.h)
    }
  }
  
  out
}

reliability_report <- function(X, assignment_df, label) {
  reps <- list()
  for (f in sort(unique(assignment_df$primary_factor))) {
    vars <- assignment_df |> filter(primary_factor == f, keep) |> pull(variable)
    res <- compute_reliability_for_factor(X, vars)
    reps[[paste0("F", f)]] <- c(list(factor = paste0("F", f), sample = label, n_items = res$n_items), res[setdiff(names(res), "n_items")])
  }
  
  # bind rows (fill missing)
  all_names <- unique(unlist(lapply(reps, names)))
  df_out <- do.call(rbind, lapply(reps, function(x) {
    x[setdiff(all_names, names(x))] <- NA
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
  tibble::as_tibble(df_out)
}

if (config$efa$engine == "psych") {
  L_train <- as.matrix(unclass(efa_train$loadings))
  assignment <- assign_items_to_factors(L_train)
  write.csv(assignment, file.path(out_dir, "item_to_factor_assignment_training_rules.csv"), row.names = FALSE)
  
  rel_train <- reliability_report(X_train, assignment, label = "training")
  rel_test  <- if (!is.null(X_test)) reliability_report(X_test, assignment, label = "test") else NULL
  
  rel_all <- bind_rows(rel_train, rel_test)
  
  # Print to console
  message("\n--- Reliability summary (based on training loading rules) ---")
  print(rel_all)
  
  # Save
  write.csv(rel_all, file.path(out_dir, "reliability_summary_train_and_test.csv"), row.names = FALSE)
  writexl::write_xlsx(list(reliability = rel_all), file.path(out_dir, "reliability_summary_train_and_test.xlsx"))
  
  # Also write a brief txt report
  sink(file.path(out_dir, "reliability_summary_train_and_test.txt"))
  cat("Reliability summary (based on training-derived item assignment rules)\n")
  cat("Rules: |loading| >= ", config$reliability$loading_cutoff,
      " and 2nd-highest |loading| < ", config$reliability$crossloading_cutoff, "\n\n", sep = "")
  print(rel_all)
  sink()
  
  message("Reliability saved to: ", file.path(out_dir, "reliability_summary_train_and_test.csv"))
  
} else {
  message("Reliability reporting is implemented in this pipeline for psych::fa solutions.")
}

message("\nDONE. Outputs in: ", out_dir)
