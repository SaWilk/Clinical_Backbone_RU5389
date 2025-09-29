# Load necessary library
# install.packages(c("readxl","dplyr","lubridate","stringr"))  # if needed
library(readxl)
library(dplyr)
library(lubridate)
library(stringr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
PATH_Q = file.path("..", "01_project_data", "2_backbone", "questionnaires", "2_2025-09-26_adults.xlsx");

# Read in the data (replace with your file path)
df <- read_excel(PATH_Q)

# Convert start and submit dates to POSIXct (date-time objects)
df <- df %>%
  mutate(
    startdate = as.POSIXct(startdate, format = "%Y-%m-%d %H:%M:%S"),
    submitdate = as.POSIXct(submitdate, format = "%Y-%m-%d %H:%M:%S")
  )

# Calculate duration in minutes for each participant
df <- df %>%
  mutate(duration_minutes = as.numeric(difftime(submitdate, startdate, units = "mins")))

# Compute the average duration (ignoring NAs)
average_duration <- median(df$duration_minutes, na.rm = TRUE)

# Print result
cat("Average completion time:", round(average_duration, 2), "minutes\n")


### ---- PsyToolkit: average completion time (minutes) ----
# Read PsyToolkit survey export (adjust the path)

PATH_T = file.path("..", "01_project_data", "2_backbone", "experiment_data", "2_2025-09-26_adults.xlsx");

psy <-  read_excel(PATH_T)

# Parse TIME_start / TIME_end that may look like:
# "YYYY-MM-DD HH:MM:SS" or "DD.MM.YYYY HH:MM:SS" (common in EU exports)
parse_dt <- function(x) {
  x <- str_trim(as.character(x))
  suppressWarnings(parse_date_time(
    x,
    orders = c("Y-m-d H:M:S","Y-m-d H:M",
               "d.m.Y H:M:S","d.m.Y H:M",
               "Y/m/d H:M:S","Y/m/d H:M"),
    tz = "UTC",
    exact = FALSE
  ))
}

res <- psy %>%
  mutate(
    start_ts = parse_dt(.data$TIME_start),
    end_ts   = parse_dt(.data$TIME_end),
    duration_min_raw = as.numeric(difftime(end_ts, start_ts, units = "mins"))
  ) %>%
  # basic quality control: drop negatives, ultra-short blips, and extreme outliers
  mutate(duration_min = ifelse(duration_min_raw < 0 | duration_min_raw < 0.1 | duration_min_raw > 8*60,
                               NA_real_, duration_min_raw))

valid_n <- sum(!is.na(res$duration_min))
if (valid_n == 0) {
  cat("No valid durations found. Check that TIME_start / TIME_end parse correctly.\n",
      "Example values:\n",
      head(psy[c('TIME_start','TIME_end')], 5), "\n")
} else {
  avg <- mean(res$duration_min, na.rm = TRUE)
  med <- median(res$duration_min, na.rm = TRUE)
  cat(sprintf("PsyToolkit – average: %.2f min | median: %.2f min | n=%d valid of %d\n",
              avg, med, valid_n, nrow(res)))
}
# project 3: 

# Questionnaires median completion time: 32 minutes
# PsyToolkit – average: 19.29 min | median: 18.00 min | n=95 valid of 95

# project 2:
# Questionnaires median completion time: 39.12 minutes
# PsyToolkit – average: 19.50 min | median: 19.00 min | n=38 valid of 38




