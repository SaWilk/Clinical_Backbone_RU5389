### config-Liste ###############################################################
# FHS
confidence_cutoff = 1

# BACS
allowed_errors <- 7
rt_trial_control <- F
rt_trial_abs_min <- 400
rt_trial_rel_min <- 3
rt_trial_rel_max <- 3
rt_vp_control <- F
rt_vp_rel_min <- 3
rt_vp_rel_max <- 3

# WCST & LNS
#install.packages(c("dplyr", "readr", "tidyr", "readxl", "purrr"), dependencies = TRUE)
library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(purrr)

#Tribble mit den Pfaden zu den Master-Excel-Dateien und den jeweiligen Datenverzeichnissen für jede Stichprobe
base_dir <- "data_raw" 

batch_settings <- tibble(
  sample_name = c("adults", "adolescents"),
  master_filepath = c(
    file.path(base_dir, "ALL_2026-05-21_adults_cogtests.xlsx"),
    file.path(base_dir, "ALL_2026-05-21_adolescents_cogtests.xlsx")
  ),
  data_dir = c(
    file.path(base_dir, "ALL_2026-05-21_adults_cogtests_data"),     
    file.path(base_dir, "ALL_2026-05-21_adolescents_cogtests_data") 
  )
)

# Spaltennamen in den Master-Excel-Dateien (ID und Dateinamen WCST & LNS)
col_vpid <- "id"    
col_wcst <- "WCST"   
col_lns  <- "LNS" 
col_bacs <- "BACS"

### scoring FHS ################################################################

# load packages
library("tidyverse")
library("readxl")

# read data
adults <- read.csv2("data_raw/adults_clean_master.csv") # questionnare data of adult sample
adolescents <- read.csv2("data_raw/adolescents_clean_master.csv") # questionnare data of adolescent sample
item_info <- read_excel("scoring/2025-10-28_Item_Information_Adults.xlsx") # list of all questionnare items with assignment to scale/subscale
output_data <- read_excel("data_mod/adults_adolescents_complete_subscales.xlsx")

# rename columns
FHScol_names <- item_info %>%
  filter(Scale == "FHSfamilytree") %>%
  mutate(Item = str_replace_all(Item, "\\[|\\]", "")) %>%
  pull(Item)

demographicscol_names <- item_info %>%
  filter(Scale == "demographics") %>%
  mutate(Item = str_replace_all(Item, "\\[|\\]", "")) %>%
  pull(Item) %>%
  { c(., "age_years") }


adults <- adults %>%
  rename_with(~ str_replace_all(.x, "\\.", ""))

adolescents <- adolescents %>%
  rename_with(~ str_replace_all(.x, "\\.", ""))

# keep FHS variables, project and id, add sample variable and combine samples
FHS_adults <- adults %>% # für Vergleichszwecke
  select(
    vp_id,
    project,
    any_of(FHScol_names)
  )
FHS_adolescents <- adolescents %>% # für Vergleichszwecke
  select(
    vp_id,
    project,
    any_of(FHScol_names)
  )
FHS <-
  bind_rows(
    FHS_adults %>%
      mutate(sample = "adults") %>%
      relocate(sample, .before = vp_id) %>%
      select(
        sample,
        vp_id,
        project,
        any_of(FHScol_names) # !bei verwendung von all_of: findet in quest_adu nicht alle items, die es laut item_info geben sollte
      ),
    FHS_adolescents %>%
      mutate(sample = "adolescents") %>%
      relocate(sample, .before = vp_id) %>%
      select(
        sample,
        vp_id,
        project,
        any_of(FHScol_names) # !bei verwendung von all_of: findet in quest_ado nicht alle items, die es laut item_info geben sollte
      )
  ) %>%
  mutate(sample = factor(sample)) %>%
  relocate(phobiaconfidencesibling4, .after = phobiaconfidencesibling3)

# control for duplicates in vp_id
double_vp_id_adu <- FHS_adults %>% # what to do with them?
  count(vp_id) %>%
  filter(n > 1)
double_vp_id_ado <- FHS_adolescents %>% # what to do with them?
  count(vp_id) %>%
  filter(n > 1)
double_vp_id <- FHS%>% # what to do with them?
  count(vp_id) %>%
  filter(n > 1) %>%
  filter(!vp_id %in% double_vp_id_ado$vp_id,
         !vp_id %in% double_vp_id_adu$vp_id)

# remove irrelevant variables and save relevant ones in vector
# alle mit 1 im Namen geben an, ob für einen Verwandten oder die vp selbst eine Störung vermutet wird
# schlägt sich aber auch in confidence variablen nieder, 1 variablen werden also. nicht benötigt
FHS01 <- FHS %>%
  select(
    -relativesInfo,
    -contains("1self"), -contains("1child"),
    -contains("1sibling"), -contains("1parent"),
    -contains("10")
  )
FHS01col_names <- FHS01 %>%
  select(-sample, -vp_id, -project) %>%
  names(.)

# remove vp that did not answer any of the FHS questions
vp_na <- FHS01 %>%
  filter(if_all(all_of(FHS01col_names), is.na)) %>%
  pull(vp_id)
FHS02 <- FHS01 %>%
  filter(!if_all(all_of(FHS01col_names), is.na))

# nas durch 0 ersetzen, wenn entsprechender Verwandter existiert, aber keine Diagnose angegeben wurde
FHS03 <- FHS02 %>%
  mutate(
    across(contains("confidence"), ~ {
      cn <- cur_column()
      
      # self: immer NA -> 0
      if (grepl("confidenceself$", cn)) {
        return(as.numeric(ifelse(is.na(.x), 0, .x)))
      }
      
      # rel und k aus {diagnosis}confidence{rel}{k} extrahieren
      m <- regexec("^.*confidence(parent|child|sibling)([0-9]+)$", cn)
      parts <- regmatches(cn, m)[[1]]
      
      # Robustheit: wenn kein Match, dann nichts verändern
      if (length(parts) == 0) {
        warning(sprintf("Kein Match für Spalte: %s", cn))
        return(as.numeric(.x))
      }
      
      rel <- parts[2]
      k   <- as.integer(parts[3])
      
      replace_zero <- rel == "parent" |
        (rel == "child"   & k <= children) |
        (rel == "sibling" & k <= siblings)
      
      as.numeric(ifelse(is.na(.x) & replace_zero, 0, .x))
    })
  )

# weitere Spalten, die nur nas enthalten, löschen (dann hat keine vp so viele der entsprechenden Verwandtenart, wie die id-no angibt und niemand hat etwas in das OpenText Feld eingegeben)
cols_na <- FHS03 %>%
  select(where( ~ all(is.na(.)))) %>%
  names()
FHS04 <- FHS03 %>%
  select(-all_of(cols_na))

# Evaluation der Spalte FHSOpenText
OpenText <- FHS04 %>%
  select(vp_id, FHSOpenText) %>%
  # herausfiltern von leeren Antwortfeldern
  filter(FHSOpenText != "") %>%
  # herausfiltern von Antworten nur bestehend aus Leerzeichen oder einem beliebigen Zeichen
  filter(!grepl("^.$", FHSOpenText) &
           !grepl("^\\s+", FHSOpenText)) %>% 
  # herausfiltern von Antworten, in denen Vps sagen, dass sie nichts mehr hinzuzufügen haben
  filter(!grepl("^nein", FHSOpenText, ignore.case = T) &
           !grepl("^ne", FHSOpenText, ignore.case = T) &
           !grepl("^nö", FHSOpenText, ignore.case = T) &
           !grepl("^momentan\\s*nichts", FHSOpenText, ignore.case = T)
           ) %>% 
  # herausfiltern von Quatsch-Antworten
  filter(!vp_id %in% c(20045, 70038, 70108)) %>%
  # herausfiltern von Antworten, die nicht das Thema des Fragebogens betreffen
  filter(!vp_id %in% c(30083)) %>%
  # herausfiltern von Antworten, deren Informationen nicht zu einer Neubewertung der confidence-Variablen führen
  filter(!vp_id %in% c(40033, 70046, 80072, 80075)) %>%
  # herausfiltern von Antworten, die nur einzelne Symptome beschreiben oder psychische Störungen betreffen, die nicht durch den FHS abgefragt werden
  filter(!vp_id %in% c(30065, 30086, 30101, 40020, 40050, 50049, 70235, 90029, 90012, 70120, 70166, 70090, 80052, 80073, 80074, 90015)) %>%
  # herausfiltern von Antworten, die Verwandte betreffen, die nicht durch den FHS abgefragt werden
  filter(!vp_id %in% c(20025, 20030, 20084, 70012, 70020, 90006)) %>%
  # herausfiltern von Antworten, in denen Verwandte nicht klar identifiiert werden können
  filter(!vp_id %in% c(20036, 70126)) %>%
  # herausfiltern von Antworten, in denen genannte Probleme nicht (eindeutig) als psychische Störungen kategorisiert werden könnnen
  filter(!vp_id %in% c(20062, 20083, 30036, 30073, 32073, 40052, 70003, 70014, 70215, 90011, 70065, 70091, 90004, 70091, 70176, 70193))

# Identifizierung der Verwandten relevanter Einträge anhand der Initialien (weitere im Analyse-Log)
names(FHS04)[which(FHS04[FHS04$vp_id == 30007, ] == "AW")]
names(FHS04)[which(FHS04[FHS04$vp_id == 30066, ] == "MZ")]
names(FHS04)[which(FHS04[FHS04$vp_id == 80052, ] == "L")]

# Umänderung der confidence-Variablen basierend auf den OpenText Einträgen für die entsprechenden Diagnosen und Verwandten (siehe Analysis-Log)
FHS04[FHS04$vp_id == 20014, "psyconfidenceparent1"] <- 2
FHS04[FHS04$vp_id == 20014, "sudconfidenceparent1"] <- 2
FHS04[FHS04$vp_id == 30007, "depressionconfidenceparent1"] <- 3
FHS04[FHS04$vp_id == 30007, "maniaconfidenceparent1"] <- 2
FHS04[FHS04$vp_id == 30041, "depressionconfidenceparent1"] <- 3
FHS04[FHS04$vp_id == 30066, "sudconfidenceparent2"] <- 2
FHS04[FHS04$vp_id == 50058, "sudconfidenceparent1"] <- 2
FHS04[FHS04$vp_id == 50058, "sudconfidenceparent2"] <- 2
FHS04[FHS04$vp_id == 70004, "sudconfidenceparent2"] <- 2
FHS04[FHS04$vp_id == 70231, "depressionconfidenceparent2"] <- 3
FHS04[FHS04$vp_id == 70115, "gasconfidenceself"] <- 3

# Evaluation der Spalte ownpsychdiagnother
Ownpsychdiagnother <- FHS04 %>%
  select(vp_id, ownpsychdiagnother) %>%
  # herausfiltern von leeren Antwortfeldern
  filter(ownpsychdiagnother != "") %>%
  # herausfiltern von vp_ids mit Diagnosen, die nicht im FHS abgefragt werden
  filter(!vp_id %in% c(20012, 20013, 20043, 20074, 40033, 50030, 70029,  70096, 70126, 70137, 70190, 70191, 70194, 70239, 80003, 80046, 80073, 80081, 90004, 90020, 90023, 70102, 70186))

# Umänderung der ownpsychdiagn-Variablen auf den Einträgen in ownpsychdiagnother
FHS04[FHS04$vp_id == 20014, "ownpsychdiagnPsy"] <- "Y"
FHS04[FHS04$vp_id == 20023, "ownpsychdiagnPsy"] <- "Y"
FHS04[FHS04$vp_id == 32071, "ownpsychdiagnMDE"] <- "Y"
FHS04[FHS04$vp_id == 80002, "ownpsychdiagnMDE"] <- "Y"
FHS04[FHS04$vp_id == 90015, "ownpsychdiagnPsy"] <- "Y"

# Anzahl der Diagnosen der VP
FHS05 <- FHS04 %>%
  rowwise() %>%
  mutate(
    numofalldiagnself = sum(c_across(matches("confidenceself")) >= confidence_cutoff),
    numoftreateddiagnself = sum(grepl(".+", c_across(
      matches("ownpsychdiagn")
    )))
  ) %>%
  ungroup()

# Anzahl der Diagnosen pro relative der vp
FHS06 <- FHS05 %>%
  mutate(numofdiagnparent1 = 
           rowSums(across(matches("confidenceparent1")) >= confidence_cutoff
  )) %>%
  mutate(numofdiagnsparent2 = 
           rowSums(across(matches("confidenceparent2")) >= confidence_cutoff
           ))
for (k in 1:9) {
  FHS06 <- FHS06 %>%
    mutate(
      !!paste0("numofdiagnsibling", k) :=
        {
          x <- across(matches(paste0("confidencesibling", k)))
          ifelse(
            rowSums(!is.na(x)) == 0,
            NA,
            rowSums(x >= confidence_cutoff, na.rm = TRUE)
          )
        }
    )
}
for (k in 1:9) {
  FHS06 <- FHS06 %>%
    mutate(
      !!paste0("numofdiagnchild", k) :=
        {
          x <- across(matches(paste0("confidencechild", k)))
          ifelse(
            rowSums(!is.na(x)) == 0,
            NA,
            rowSums(x >= confidence_cutoff, na.rm = TRUE)
          )
        }
    )
}

# Anzahl der relatives mit Diagnose pro Vp
FHS07 <- FHS06 %>%
  mutate(
    parentswithdiagn = 
      rowSums(across(matches("numofdiagnparent")) >= 1
              ),
    siblingswithdiagn = 
      {
        x <- across(matches("numofdiagnsibling"))
        ifelse(
          rowSums(!is.na(x)) == 0, # ergibt TRUE, wenn vp keine geschwister hat
          NA, # wenn kein Unterschied zwischen "keine Geschwister" und "keine Geschwister ohne Diagnose" gemacht werden soll, NA durch 0 ersetzen
          rowSums(x >= 1, na.rm = TRUE)
      )
    },
    childrenwithdiagn = 
      {
        x <- across(matches("numofdiagnchild")) 
        ifelse(
          rowSums(!is.na(x)) == 0, # ergibt TRUE, wenn vp keine geschwister hat
          NA, # wenn kein Unterschied zwischen "keine Kinder" und "keine Kinder ohne Diagnose" gemacht werden soll, NA durch 0 ersetzen
          rowSums(x >= 1, na.rm = TRUE)
        )
      },
    rel = rowSums(across(.cols = c("siblings", "children"))) + 2,
    relwithdiagn = 
      rowSums(across(matches("numofdiagn") & !matches(c("self", "own"))) >= 1, na.rm = T
      )
    ) %>%
  relocate(siblings, .before = siblingswithdiagn) %>%
  relocate(children, .before = childrenwithdiagn)

# Liste der Diagnosen pro Person
# vp- und relatives-Variablen finden
names_rel <- "^(.*)confidence(parent|sibling|child)([1-9])$"
names_self <- "^(.*)confidenceself$"
names_own <- "^ownpsychdiagn(.*)$"
cols_rel  <- names(FHS07)[str_detect(names(FHS07), names_rel)]
cols_self <- names(FHS07)[str_detect(names(FHS07), names_self)]
cols_own <- names(FHS07)[str_detect(names(FHS07), names_own)]

# Umwandlung in long-Format und Erstellung Verwandten sowie Diagnosen-Spalte
# Für Verwandte
long_rel <- FHS07 %>%
  pivot_longer(
    cols = all_of(cols_rel),
    names_to = "variablename",
    values_to = "confidence"
  ) %>%
  mutate(
    diagnose = factor(str_match(variablename, names_rel)[, 2], 
                      levels = unique(str_match(variablename, names_rel)[, 2])),
    rel       = factor(str_match(variablename, names_rel)[, 3],
                       levels = c("parent", "sibling", "child")),
    k         = str_match(variablename, names_rel)[, 4]
    ) %>%
  select(vp_id, rel, k, diagnose, confidence)

# Für self Variable
long_self_all <- FHS07 %>%
  pivot_longer(
    cols = all_of(cols_self),
    names_to = "variablename",
    values_to = "confidence"
  ) %>%
  mutate(
    diagnose  = str_match(variablename, names_self)[, 2],
    rel = "self",
    k = "0"
  ) %>%
  select(vp_id, rel, k, diagnose, confidence)

# Für own Variable
long_self_treated <- FHS07 %>%
  pivot_longer(
    cols = all_of(cols_own),
    names_to = "variablename",
    values_to = "diagnexistence"
  ) %>%
  mutate(
    diagnose = factor(str_match(variablename, names_own)[, 2], 
                      levels = unique(str_match(variablename, names_own)[, 2])),
    rel = "self",
    k = "0"
  ) %>%
  select(vp_id, rel, k, diagnose, diagnexistence)

# Anpassung der Diagnosekürzel von own-Variablen an die der self-Variablen
# unique(long_self_all$diagnose)
# unique(long_self_treated$diagnose)
# long_self_treated <- long_self_treated %>%
#   mutate(diagnose = fct_recode(diagnose, "depression" = "MDE", "mania" = "Bipolar")) %>% # sollte Bipolar zu mania gemacht werden? bipolar ist etwas allgemeiner als Frage nach manischer Episode?
#   mutate(diagnose = fct_relabel(diagnose, ~ tolower(.x)))
  

# Zusammenführen der long-dfs
long_all <- bind_rows(
  select(long_rel, vp_id, rel, k, diagnose),
  select(long_self_all, vp_id, rel, k, diagnose),
  select(long_self_treated, vp_id, rel, k, diagnose)
)

# Anzahl der Verwandten pro Diagnose (hier braucht es keine na und 0 Unterscheidung, weil jede vp mind. 2 relatives (parents) hat)
rel_per_diagn <- long_rel %>%
  filter(confidence >= confidence_cutoff) %>%
  group_by(vp_id, diagnose) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = diagnose,
    names_expand = T,
    values_from = n,
    values_fill = 0,
    names_prefix = "relwith"
  )

diagnoses_names <- long_rel %>%
  pull(diagnose) %>%
  as.character() %>%
  unique() %>%
  paste0("relwith", .)

FHS08 <- FHS07 %>%
  left_join(rel_per_diagn, by = "vp_id") %>%
  mutate(across(all_of(contains(diagnoses_names)), ~ ifelse(is.na(.), 0, as.numeric(.))))

# Anteil der Verwandten pro Diagnose an der Gesamtzahl an Verwandten
FHS09 <- FHS08 %>%
  mutate(across(
    .cols = starts_with("relwith"),
    .fns  = ~ .x / rel,
    .names = "prop{.col}"
  ))

# Liste der Diagnosen pro Verwandte/r
diagn_per_rel <- long_rel %>%
  group_by(vp_id, rel, k) %>%
  summarise(diagnoses = {
    exist <- confidence >= 1
    # Nur diagnose nehmen, bei denen confidence >=1 ist
    diagn <- unique(diagnose[exist])
    # Wenn für alle diagnose confidence NA war -> Ergebnis NA, denn Verwandter existiert nicht
    if (all(is.na(confidence))) {
      NA_character_ 
    } 
    # Wenn überall 0, dann existiert Verwandter, aber hat keine Diagnose
    else if (length(diagn) == 0) {
      "nodiagn"
    } 
    # Wenn Diagnosen vorhanden, entsprechende in die Zelle schreiben
    else {
      paste(diagn, collapse = ", ")
    }
  }, .groups = "drop") %>%
  pivot_wider (
    names_from = c(rel, k),
    names_prefix = "alldiagn",
    names_sep = "",
    values_from = diagnoses
  )
  

# Liste der Diagnosen für self-Variable
diagn_self_all <- long_self_all %>%
  group_by(vp_id, rel) %>%
  summarise(diagnoses = {
    exist <- confidence >= 1
    # Nur diagnose nehmen, bei denen confidence>=1 ist
    diagn <- unique(diagnose[exist])
    # Wenn überall 0, dann existiert Verwandter, aber hat keine Diagnose
    if (length(diagn) == 0) {
      "nodiagn"
    } 
    # Wenn Diagnosen vorhanden, entsprechende in die Zelle schreiben
    else {
      paste(diagn, collapse = ", ")
    }
  }, .groups = "drop") %>%
  pivot_wider (
    names_from = rel,
    names_prefix = "alldiagn",
    names_sep = "",
    values_from = diagnoses
  )

# Liste der Diagnosen für own-Variablen
diagn_self_treated <- long_self_treated %>%
  group_by(vp_id, rel) %>%
  summarise(
    diagnoses = {
      diagn <- unique(diagnose[diagnexistence == "Y"])
      # other <- unique(diagnexistence[grepl("^.+$", diagnexistence) & diagnexistence != "Y"])
      
      comb <- c(
        if (length(diagn) > 0) paste(diagn, collapse = ", ") else character(0)
        # ,
        # if (length(other) > 0) paste(other, collapse = ", ") else character(0)
      )
      
      if (length(comb) == 0) "nodiagntreated" else paste(comb, collapse = ", ")
    },
    .groups = "drop"
  ) %>%
  pivot_wider (
    names_from = rel,
    names_prefix = "treateddiagn",
    names_sep = "",
    values_from = diagnoses
  )

# Kombinieren mit quest_FHS
FHS10 <- FHS09 %>%
  left_join(diagn_per_rel, by = "vp_id") %>%
  left_join(diagn_self_all, by = "vp_id") %>%
  left_join(diagn_self_treated, by = "vp_id")

# Export für Überprüfung
output_FHS <- FHS10 %>%
  select(-parents001, -parents002, - contains("names"), -contains("contact"), -contains("own"), -FHSOpenText)
write_csv2(output_FHS, "data_mod/output_FHS.csv")

### scoring BACS ###############################################################
# load packages
library("tidyverse")
library("readxl")

### read complete subscales data
compl_subscales <- read_excel("data_mod/adults_adolescents_complete_subscales.xlsx")
compl_subscales$vp_id <- as.numeric(compl_subscales$vp_id)

### read meta data
master_excel_adu <- read_excel("data_raw/ALL_2026-05-21_adults_cogtests.xlsx") %>%
  rename(vp_id = id) %>%
  relocate(vp_id, .before = participant) %>%
  mutate(filepath_BACS = paste0("data_raw/ALL_2026-05-21_adults_cogtests_data/", BACS))
master_excel_ado <- read_excel("data_raw/ALL_2026-05-21_adolescents_cogtests.xlsx") %>%
  rename(vp_id = id) %>%
  relocate(vp_id, .before = participant) %>%
  mutate(filepath_BACS = paste0("data_raw/ALL_2026-05-21_adolescents_cogtests_data/", BACS))
master_excel <- bind_rows(
  master_excel_ado,
  master_excel_adu
    )


### read bacs data
col_names <- c(
  "project",
  "vp_id",
  "block",
  "TRIALCOUNT",
  "NumberCorrect",
  "STATUS",
  "answer",
  "RT",
  "correct_count",
  "error_count",
  "time_trialstart"
)

# files according to master excel
bacs_ado_filepaths_master <- master_excel_ado$filepath_BACS
bacs_ado_files_master <- bacs_ado_filepaths_master %>%
  map(~possibly(read_table, otherwise = NULL)(.x, col_names = col_names))
bacs_ado_master <- bacs_ado_files_master %>%
  compact() %>%
  bind_rows()

# all files
bacs_ado_filepaths <- list.files(path = "data_raw/ALL_2026-05-21_adolescents_cogtests_data",
                                 pattern = "^RU5389_BACS_adol.*\\.txt$",
                                 full.names = TRUE)
bacs_ado <- bacs_ado_filepaths %>%
  map(read_table, col_names = col_names) %>%
  bind_rows()

# comparison of number of filepaths
length(bacs_ado_filepaths)
length(bacs_ado_filepaths_master)

# comparison of number of files actually read in
bacs_ado %>%
  filter(block == "training") %>%
  nrow() / 7
bacs_ado_master %>%
  filter(block == "training") %>%
  nrow() / 7
bacs_ado_missing_files <- sum(map_lgl(bacs_ado_files_master, is.null))

# control for differences in IDs
setdiff(bacs_ado$vp_id, bacs_ado_master$vp_id)
setdiff(bacs_ado_master$vp_id, bacs_ado$vp_id) 

# files according to master excel
bacs_adu_filepaths_master <- master_excel_adu$filepath_BACS
bacs_adu_files_master <- bacs_adu_filepaths_master %>%
    map(~possibly(read_table, otherwise = NULL)(.x, col_names = col_names))
bacs_adu_master <- bacs_adu_files_master %>%
  compact() %>%
  bind_rows()

# all files
bacs_adu_filepaths <- list.files(path = "data_raw/ALL_2026-05-21_adults_cogtests_data",
                                 pattern = "^RU5389_BACS.*\\.txt$",
                                 full.names = TRUE)
bacs_adu <- bacs_adu_filepaths %>%
  map(read_table, col_names = col_names) %>%
  bind_rows()

# comparison of number of filepaths
length(bacs_adu_filepaths)
length(bacs_adu_filepaths_master)

# comparison of number of files actually read in
bacs_adu %>%
  filter(block == "training") %>%
  nrow() / 7
bacs_adu_master %>%
  filter(block == "training") %>%
  nrow() / 7
bacs_adu_missing_files <- sum(map_lgl(bacs_adu_files_master, is.null))

# control for differences in IDs
setdiff(bacs_adu$vp_id, bacs_adu_master$vp_id)
setdiff(bacs_adu_master$vp_id, bacs_adu$vp_id)


# combine adults and adolescents
bacs <- bind_rows(
  bacs_adu_master %>%
    mutate(sample = "adults") %>%
    relocate(sample, .before = project),
  bacs_ado_master %>%
    mutate(sample = "adolescents") %>%
    relocate(sample, .before = project)
) %>%
  mutate(sample = factor(sample),
         answer = gsub('"', '', answer), # remove "" in variable answer
         STATUS = ifelse(STATUS == 2, 0, STATUS))

# control for duplicates in vp_id
double_vp_id_adu <- bacs_adu_master %>% # what to do with them?
  filter(block == "training") %>%
  count(vp_id) %>%
  mutate(n = n/7) %>%
  filter(n > 1)
double_vp_id_ado <- bacs_ado_master %>% # what to do with them?
  filter(block == "training") %>%
  count(vp_id) %>%
  mutate(n = n/7) %>%
  filter(n > 1)
double_vp_id <- bacs %>% # what to do with them?
  filter(block == "training") %>%
  count(vp_id) %>%
  mutate(n = n/7) %>%
  filter(n > 1) %>%
  filter(!vp_id %in% double_vp_id_ado$vp_id,
         !vp_id %in% double_vp_id_adu$vp_id)

# vp with too many errors in training block (missing comprehension?)
vp_no_compr <- bacs %>%
  filter(block == "training") %>%
  group_by(vp_id) %>%
  summarise(errors = sum(STATUS == 2)) %>%
  filter(errors > allowed_errors) %>%
  pull(vp_id)

bacs <- bacs %>%
  filter(!vp_id %in% vp_no_compr)

# control for missing/weird answers
symbols <- bacs %>%
  filter(!answer %in% c(0:9) & answer != "")
na <- bacs %>%
  filter(answer == "")

bacs01 <- bacs %>%
  mutate(
    STATUS = case_when(
      bacs$NumberCorrect == 1 & bacs$answer == "!" |
        bacs$NumberCorrect == 2 & bacs$answer == '"' |
        bacs$NumberCorrect == 3 & bacs$answer == "§" |
        bacs$NumberCorrect == 4 & bacs$answer == "$" |
        bacs$NumberCorrect == 5 & bacs$answer == "%" |
        bacs$NumberCorrect == 6 & bacs$answer == "&" |
        bacs$NumberCorrect == 7 & bacs$answer == "/" |
        bacs$NumberCorrect == 8 & bacs$answer == "(" |
        bacs$NumberCorrect == 9 & bacs$answer == ")" |
        bacs$NumberCorrect == 0 & bacs$answer == "=" # symbols match the number on the keyboard when used while holding shift
      ~ 1,
      TRUE ~ STATUS
    )
  )

symbols01 <- bacs01 %>%
  filter(!answer %in% c(0:9) & answer != "")
na01 <- bacs01 %>%
  filter(answer == "")

# drop training trials
bacs02 <- bacs01 %>%
  filter(block != "training")

# drop trials with too low or high rt within vp
bacs03 <- {
  if (rt_trial_control == T) {
    
    outlier_trials <- bacs02 %>%
      group_by(vp_id) %>%
      mutate(mean = mean(RT), sd = sd(RT)) %>%
      filter(RT < rt_trial_abs_min |
               RT < rt_trial_rel_min * sd - mean |
               RT > rt_trial_rel_max * sd + mean) %>%
      select(-sd, -mean)
    
    bacs02 %>%
      anti_join(outlier_trials, by = names(bacs01))
  }
  else {
    bacs02
  }
}

# drop vp with too low or high mean rt
bacs04 <- {
  if (rt_vp_control == T) {
    
    outlier_vp <- bacs03 %>%
      mutate(mean_all = mean(RT), sd_all = sd(RT)) %>%
      group_by(vp_id) %>%
      filter(mean(RT) < rt_vp_rel_min * sd_all - mean_all |
               mean(RT) > rt_vp_rel_max * sd_all + mean_all) %>%
      pull(vp_id) %>%
      unique()
    
    bacs03 %>%
      filter(!vp_id %in% outlier_vp)
  }
  else {
    bacs03
  }
}

# average rt, correct trials and total trials
output_bacs <- bacs04 %>%
  group_by(vp_id) %>%
  summarise(sample = unique(sample),
            correct_trials = sum(STATUS == 1),
            wrong_trials = sum(STATUS == 0),
            total_trials = n(),
            mean_rt = mean(RT)) %>%
  relocate(sample, .before = vp_id)

# Export alone
write_csv2(output_bacs, "data_mod/output_bacs.csv")

# export in complete_subscales
comb_bacs <- output_bacs %>%
  mutate(score_bacs = correct_trials) %>%
  select(vp_id, score_bacs)

compl_subscales_bacs <- compl_subscales %>%
  left_join(comb_bacs, by = "vp_id", unmatched = "error")

compl_subscales_bacs <- compl_subscales %>%
  left_join(comb_bacs, by = "vp_id", unmatched = "drop") %>%
  relocate(score_bacs, .before = z_score_aps)

write_csv2(compl_subscales_bacs, 
                 "data_mod/adults_adolescents_complete_subscales_bacs.csv")

## scoring WCST ###############################################################
#A) Daten einlesen und Spalten benennen
score_wcst_single <- function(filepath, sample_name, excel_id) {
  # TRACKING: Wenn Datei fehlt, trotzdem Proband zurückgeben, aber mit Status
  if (!file.exists(filepath)) {
    return(tibble(sample = sample_name, vp_id = as.character(excel_id), Status_WCST = "Fehlt im Ordner"))
  }
  
  col_names <- c("project", "vpid", "card", "ShapeCorrect", "NumberCorrect", 
                 "ColorCorrect", "RT", "STATUS", "answer", "anyerror", 
                 "perseverationerror", "nonperseverationerror", "correct_count", 
                 "block_count", "trial_count", "time_trialstart")
  df <- read_table(filepath, col_names = col_names, show_col_types = FALSE) #verarbeitet automatisch beliebige whitespaces
  
  # TRACKING: Datei war kaputt oder unlesbar
  if (is.null(df) || nrow(df) == 0) {
    return(tibble(sample = sample_name, vp_id = as.character(excel_id), Status_WCST = "Datei leer/Lesefehler"))
  }
  
  #B) Dimensions-Matches bestimmen
  #prüfen, ob die Antwort der korrekten Karte für die jeweilige Dimension entspricht
  df <- df %>%
    mutate(
      match_shape = ifelse(answer == ShapeCorrect, 1, 0),
      match_number = ifelse(answer == NumberCorrect, 1, 0),
      match_color = ifelse(answer == ColorCorrect, 1, 0),
      match_total = match_shape + match_number + match_color,
      is_unambiguous = match_total == 1,
      is_ambiguous = match_total > 1
    )
  
  #C) Trial-by-trial Perservations-Logik implementieren
  df$is_pers_resp <- FALSE
  df$pers_principle_active <- NA_character_
  current_principle <- NA_character_
  consecutive_new_errors <- 0
  candidate_principle <- NA_character_
  current_block <- df$block_count[1] # aktuellen Block tracken
  
  for (i in 1:nrow(df)) {
    #Bei Blockwechsel (neue Kategorie) die Perservations-Regeln zurücksetzen
    if (df$block_count[i] != current_block) {
      current_principle <- NA_character_
      candidate_principle <- NA_character_
      consecutive_new_errors <- 0
      current_block <- df$block_count[i]
    }
    
    is_error <- df$anyerror[i] == 1
    unambiguous <- df$is_unambiguous[i]
    
    # welche Dimenstion wurde bei eindeutigen Antworten gewählt?
    matched_dim <- NA_character_
    if (unambiguous) {
      if (df$match_shape[i] == 1) matched_dim <- "shape"
      if (df$match_number[i] == 1) matched_dim <- "number"
      if (df$match_color[i] == 1) matched_dim <- "color"
    }
    
    # Regel: Prinzip etablieren oder wechseln (nur bei eindeutigen Fehlern)
    if (is_error && unambiguous) {
      if (is.na(current_principle)) {
        #erstes mal etablieren, zählt nicht als Perservation
        current_principle <- matched_dim
      } else if (matched_dim == current_principle) {
        #Perservationsfehler
        df$is_pers_resp[i] <- TRUE
        consecutive_new_errors <- 0 #reset des Shift-counters 
      } else {
        #eindeutiger Fehler auf ein ANDERES Prinzip, Shift-Counter hochzählen
        if (is.na(candidate_principle) || candidate_principle != matched_dim) {
          candidate_principle <- matched_dim
          consecutive_new_errors <- 1
        } else {
          consecutive_new_errors <- consecutive_new_errors + 1
        }
        #Regel: 3 aufeinanderfolgende Fehlern -> Shift auf das neue Prinzip
        if (consecutive_new_errors == 3) {
          current_principle <- candidate_principle
          candidate_principle <- NA_character_
          consecutive_new_errors <- 0
        }
      }
    } else if (unambiguous && !is_error) {
      #bei eindeutigen richtigen Antworten wird der Shift-counter zurückgesetzt
      consecutive_new_errors <- 0
    }
    #Prinzip für späteren Sandwich-Check speichern
    df$pers_principle_active[i] <- current_principle
  }
  
  # Sandwich-Check für Ambiguous Trials
  # eine mehrdeutige Anwort ist perservativ, wenn sie vom gleichen Prinzip "eingeklemmt" ist
  for (i in 1:nrow(df)) {
    if (df$is_ambiguous[i] && !is.na(df$pers_principle_active[i])) {
      prinzip <- df$pers_principle_active[i]
      current_b <- df$block_count[i] #aktueller Block wird für die Suche nach eindeutigen Antworten gespeichert, 
      #damit das Skript nicht über Kategoriengrenzen nach P-fehlern sucht 
      
      
      #prüft, ob die aktuelle Antwort auf dem aktiven Prinzip basiert (z.B. Shape, Number, Color)
      match_on_active <- FALSE
      if (prinzip == "shape" && df$match_shape[i] == 1) match_on_active <- TRUE
      if (prinzip == "number" && df$match_number[i] == 1) match_on_active <- TRUE
      if (prinzip == "color" && df$match_color[i] == 1) match_on_active <- TRUE
      
      if (match_on_active) {
        #finde vorherige und nächste EINDEUTIGE Antwort im gleichen Block, bei der Suche nach der vorherigen und nächsten eindeutigen Antwort
        # ('prev_idx_candidates' und 'next_idx_candidates') wird strikt gefiltert, dass diese zwingend im selben Block liegen müssen.
        prev_idx_candidates <- which(df$is_unambiguous[1:(i-1)] & df$block_count[1:(i-1)] == current_b)
        next_idx_candidates <- which(df$is_unambiguous[(i+1):nrow(df)] & df$block_count[(i+1):nrow(df)] == current_b)
        
        prev_unambig_idx <- if(length(prev_idx_candidates) > 0) max(prev_idx_candidates) else NA
        next_unambig_idx <- if(length(next_idx_candidates) > 0) min(next_idx_candidates) + i else NA
        
        if (!is.na(prev_unambig_idx) && !is.na(next_unambig_idx)) {
          #sind beide eindeutige Antworten davor und danach perservativ?
          if (df$is_pers_resp[prev_unambig_idx] && df$is_pers_resp[next_unambig_idx]) {
            #ist das Prinzip dazwischen durchgängig gleich?
            if (df$pers_principle_active[prev_unambig_idx] == prinzip && 
                df$pers_principle_active[next_unambig_idx] == prinzip) {
              df$is_pers_resp[i] <- TRUE
            }
          }
        }
      }
    }
  }
  
  # D) Finale Scores berechnen
  
  # Basis Scores
  score_correct <- sum(df$anyerror == 0)
  score_errors <- sum(df$anyerror == 1)
  score_pers_err <- sum(df$is_pers_resp & df$anyerror == 1)
  score_nonpers_err <- score_errors - score_pers_err
  
  # Kategorien und Trials
  score_cat_comp <- sum(df$correct_count == 10)
  trial_first_cat <- suppressWarnings(min(which(df$correct_count == 10)))
  if (!is.finite(trial_first_cat)) trial_first_cat <- 65
  
  # Failure to maintain set (>= 5 richtig, dann Fehler, vor Vollendung der Kategorie)
  score_failure_maintain <- 0
  for (i in 2:nrow(df)) {
    if (df$correct_count[i-1] >= 5 && df$correct_count[i-1] < 10 && df$correct_count[i] == 0) {
      score_failure_maintain <- score_failure_maintain + 1
    }
  }
  
  # Conceptual Level Response (Serien von >= 3 korrekten Antworten) 
  runs <- rle(df$anyerror == 0)
  score_conceptual <- sum(runs$lengths[runs$values == TRUE & runs$lengths >= 3])
  
  # Learning to Learn (L2L) Score: mittlere Verbesserung der Fehlerquote über die Kategorien hinweg
  # Percent Error Score (PES) pro Kategorie berechnen (PES=(errors / trials) * 100)
  cat_stats <- df %>%
    group_by(block_count) %>%
    summarise(
      trials = n(),
      errors = sum(anyerror == 1),
      max_correct = max(correct_count),
      .groups = "drop"
    ) %>%
    # nur einbeziehen, wenn Kategorie vollendet (max_correct >= 10) ODER mind. 10 Trials versucht 
    filter(max_correct >= 10 | trials >= 10) %>%
    mutate(pes = (errors / trials) * 100)
  
  score_l2l <- NA_real_
  if (nrow(cat_stats) >= 3) {
    # Differenzen aufeinanderfolgender Kategorien (PES1-PES2, PES2-PES3, ...) berechnen und mitteln
    diffs <- -diff(cat_stats$pes) #negative Differenz, laut Manual Cat 1-Cat2
    score_l2l <- mean(diffs)
  }
  
  # NA-Tracking der Rohdaten (Zählt fehlende Antworten)
  WCST_NA_Trials <- sum(is.na(df$answer))
  
  # E) Output als 1-Zeilen Tribble strukturieren 
  output <- tibble(
    sample = sample_name,
    vp_id = as.character(excel_id),
    Status_WCST = "Erfolgreich", # TRACKING: Alles hat geklappt
    score_WCST_correct = score_correct,
    score_WCST_errors = score_errors,
    score_WCST_pers_err = score_pers_err,
    score_WCST_nonpers_err = score_nonpers_err,
    score_WCST_cat_comp = score_cat_comp,
    score_WCST_trials_first_cat = trial_first_cat,
    score_WCST_fail_maintain = score_failure_maintain,
    score_WCST_conceptual_resp = score_conceptual,
    score_WCST_L2L = score_l2l
  )
  return(output)
}

# F) BATCH- WRAPPER 
# die Master-Excel wird direkt als Tabelle genutzt, um Verrutschen und Duplikate zu verhindern
process_wcst_sample <- function(sample_name, master_filepath, data_dir) {
  cat("\nStarte WCST Verarbeitung für:", toupper(sample_name), "\n")
  
  # Masterdatei einlesen, Dateinamen und VPIDs extrahieren,
  # mit dynamischen Spaltennamen (statt Spaltennummern) für Flexibilität
  master_df <- read_excel(master_filepath) %>%
    select(excel_vpid = all_of(col_vpid), filename = all_of(col_wcst)) %>%
    filter(!is.na(filename) & filename != "")
  
  pmap_dfr(master_df, function(excel_vpid, filename) {
    score_wcst_single(file.path(data_dir, filename), sample_name, excel_vpid)
  })
}


### scoring LNS ###
# A) EINLESEN UND VORBEREITUNG DER DATEN
score_lns_single <- function(filepath, sample_name, excel_id) {
  # TRACKING: Datei fehlt
  if (!file.exists(filepath)) {
    return(tibble(sample = sample_name, vp_id = as.character(excel_id), Status_LNS = "Fehlt im Ordner"))
  }
  
  # Einlesen der Daten 
  raw_data <- tryCatch({ 
    read.table(filepath, header = FALSE, fill = TRUE, stringsAsFactors = FALSE) 
  }, error = function(e) return(NULL))
  
  # TRACKING: Lesefehler
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    return(tibble(sample = sample_name, vp_id = as.character(excel_id), Status_LNS = "Datei leer/Lesefehler"))
  }
  
  # Spalten Definieren gemäß LNS.txt und Zuweisung (deckt den Fall ab, falls mehr/weniger Spalten vorhanden sind)
  col_names <- c("project", "vpid", "block_type", "block_num",
                 "itemCorrect", "b1", "b2", "b3", "b4", "b5",
                 "b6", "b7", "b8", "b9", "rt", "seq_correct",
                 "correct_count", "span_error", "time_start")
  actual_cols <- min(length(col_names), ncol(raw_data))
  colnames(raw_data)[1:actual_cols] <- col_names[1:actual_cols]
  
  # Filtern auf Experiment-Trials (Trainingstrial werden ausgeschlossen)
  exp_data <- raw_data %>% filter(block_type == "experiment")
  
  # TRACKING: Keine Experiment-Daten in der Datei
  if (nrow(exp_data) == 0) {
    return(tibble(sample = sample_name, vp_id = as.character(excel_id), Status_LNS = "Keine Experiment-Trials"))
  }
  
  # B) DEKODIERUNG DER ANTWORTEN (1-26 = A-Z, >26 = Zahl)
  decode_lns_response <- function(vals) {
    vals <- as.numeric(vals)
    vals <- vals[!is.na(vals) & vals > 0]
    if (length(vals) == 0) return("")
    decoded <- ifelse(vals <= 26, LETTERS[vals], as.character(vals - 26))
    return(paste(decoded, collapse = ""))
  }
  exp_data <- exp_data %>%
    rowwise() %>%
    mutate(response_decoded = decode_lns_response(c_across(b1:b9))) %>%
    ungroup()
  
  # C) Sicherheitsnetz: Abbruch nach 4 Fehlern in Folge
  stop_index <- which(as.numeric(exp_data$span_error) >= 4)[1]
  if (!is.na(stop_index)) {
    exp_data <- exp_data[1:stop_index, ]
  }
  
  # D) SCORING LOGIC
  #seq_correct: 1 = richtig, 0 = falsch
  # 1) Total Score: Summe der korrekten Experiment-Trials 
  score_correct <- sum(as.numeric(exp_data$seq_correct) == 1, na.rm = TRUE)
  
  # 2) Fehler Gesamt
  score_errors  <- sum(as.numeric(exp_data$seq_correct) == 0, na.rm = TRUE)
  
  # 3) Max span length: die längste korrekt erinnerte Sequenz
  # Länge der Sequenz = Anzahl der Zeichen in "itemCorrect" (z.B. "6D" = 2) 
  exp_data <- exp_data %>% mutate(span_length = nchar(as.character(itemCorrect)))
  correct_trials <- exp_data %>% filter(seq_correct == 1)
  max_span <- ifelse(nrow(correct_trials) > 0, max(correct_trials$span_length, na.rm = TRUE), 0)
  
  # mean reaction time für korrekte Trials (nützlich als Sekundärveriable)
  mean_rt  <- ifelse(nrow(correct_trials) > 0, mean(as.numeric(correct_trials$rt), na.rm = TRUE), NA)
  
  # NA-Tracking der Rohdaten
  LNS_NA_Trials <- sum(is.na(exp_data$seq_correct))
  
  # E) OUTPUT GENERIEREN (1-zeiliges Tibble, dient als Merge-Key mit der Master-Excel)
  output <- tibble(
    sample = sample_name,
    Status_LNS = "Erfolgreich", # TRACKING: Alles hat geklappt!
    vp_id = as.character(excel_id),
    score_LNS_correct = score_correct,
    score_LNS_errors = score_errors,
    score_LNS_max_span = max_span,
    score_LNS_mean_rt_correct = round(mean_rt, 2)
  )
  return(output)
}

# F) BATCH- WRAPPER
# die Master-Excel wird direkt als Tabelle genutzt, um Verrutschen und Duplikate zu verhindern
process_lns_sample <- function(sample_name, master_filepath, data_dir) {
  cat("\nStarte LNS Verarbeitung für:", toupper(sample_name), "\n")
  
  # daten vorbereiten
  master_df <- read_excel(master_filepath) %>%
    select(excel_vpid = all_of(col_vpid), filename = all_of(col_lns)) %>%
    filter(!is.na(filename) & filename != "")
  # PMAP-Schleife: Für jede Zeile in der Masterdatei die Funktion score_lns_single aufrufen und Ergebnisse zusammenführen
  pmap_dfr(master_df, function(excel_vpid, filename) {
    score_lns_single(file.path(data_dir, filename), sample_name, excel_vpid)
  })
}
# ==============================================================================
# WCST & LNS: BATCH-AUSFÜHRUNG & ZUSAMMENFÜHRUNG 
# ==============================================================================
# A) WCST berechnen
raw_wcst_data <- pmap_dfr(batch_settings, process_wcst_sample)
# HINWEIS FÜR BACKBONE-PIPELINE: 
# der distinct-Befehl entfernt aktuell fehlerhafte Duplikate
# sobald dieses Skript in die Haupt-Pipeline integriert wird, kann dieser Filter 
# gelöscht werden, da die Pipeline ein eigenes Skript für das Duplikat-Handling besitzt
clean_wcst <- raw_wcst_data %>% distinct(sample, vp_id, .keep_all = TRUE)

# B) LNS berechnen
raw_lns_data <- pmap_dfr(batch_settings, process_lns_sample)
# HINWEIS FÜR BACKBONE-PIPELINE: Siehe WCST (kann später gelöscht werden)
clean_lns <- raw_lns_data %>% distinct(sample, vp_id, .keep_all = TRUE)

# C) Master-Datensatz erstellen (Merge)
master_cog_data <- full_join(x = clean_wcst, y = clean_lns, by = c("sample", "vp_id"))

# Wie viele Probanden haben BEIDE Tests erfolgreich absolviert?
finale_saubere_daten <- master_cog_data %>% 
  filter(Status_WCST == "Erfolgreich" & Status_LNS == "Erfolgreich")

cat("\nAnzahl vollständige Probanden im Master-Datensatz:", nrow(master_cog_data), "\n")
cat("\nAnzahl Probanden im finalen Datensatz:", nrow(finale_saubere_daten), "\n")

# Optional: Direkter Export der WCST & LNS Scores in CSV-Dateien
# write_csv(master_cog_data, "Master_Cognitive_Data_WCST_LNS_Gesamt.csv")
# write_csv(finale_saubere_daten, "Finale_Saubere_Daten_WCST_LNS_Gesamt.csv")

# ==============================================================================
# WCST & LNS: FUNNEL ANALYSE - DATENVERLUST REPORT
# ==============================================================================
cat(">> 1. Aus der Master-Excel eingelesen (bereits de-dupliziert):\n")
cat("WCST (einzigartige IDs):", nrow(clean_wcst), "\n")
cat("LNS (einzigartige IDs):", nrow(clean_lns), "\n\n")

cat(">> 2. Was ist beim Auslesen der Dateien passiert?\n")
print(master_cog_data %>% count(Status_WCST, name = "Anzahl_WCST"))
cat("\n")
print(master_cog_data %>% count(Status_LNS, name = "Anzahl_LNS"))
