# CT Motor-Vehicle trends in crashes / injuries for New Haven and
# Waterbury vs. other cities and Connecticut overall, 2015 onward
#
# Six indicators = 3 metrics x 2 road-user groups:
#   Metrics:  (1) total crashes
#             (2) fatal crashes            -> Crash Severity == "K"
#             (3) injury-or-fatal crashes  -> Crash Severity in {"A","K"}
#   Groups:   Driver/Passenger            -> no non-motorist involved
#             Pedestrian/Cyclist/Other    -> >=1 non-motorist involved

library(tidyverse)

# Locate the raw exports

# Each query exports THREE linked tables, not one split file:
#   _0 = crash-level (one row per crash)   <- what this analysis uses
#   _1 = vehicle-level (one row per vehicle)
#   _2 = person-level (one row per person)
# We focus on crash-level, so only use _0

# Line 1 of every file is the query URL, so the real header is on line 2 -> skip = 1
# Files are Windows-1252 encoded (curly quotes), not UTF-8
raw_files <- list.files(pattern = "^export_\\d+_0\\.csv$")
if (length(raw_files) == 0) {
  stop(
    "No 'export_*_0.csv' crash files found in the working directory:\n  ",
    getwd(),
    "\nThe raw CSVs live in the project folder. Point R at it, e.g.:\n",
    '  setwd("/Users/alexklee/Desktop/DataHaven/crash-analysis")\n',
    "(In RStudio: Session > Set Working Directory > To Source File Location.)",
    call. = FALSE
  )
}

# The crash file has 107 columns with several repeated names (CrashId appears 3x, etc.)
# select by stable POSITION instead of name:
#   1 = CrashId, 9 = Town Name, 14 = Year, 23 = Crash Severity, 28 = Number Of Non-Motorist
crash_cols <- c(crash_id = 1L, town = 9L, year = 14L,
                severity = 23L, n_nonmotor = 28L)

read_export <- function(path) {
  read_csv(
    path,
    skip         = 1,                       # drop the query-URL line
    col_select   = all_of(crash_cols),      # named -> renames on read
    col_types    = cols(.default = col_character()),
    locale       = locale(encoding = "Windows-1252"),
    name_repair  = "minimal",  # silence dup-name repair; we select by position
    show_col_types = FALSE
  )
}

message("Reading ", length(raw_files), " export files ...")
crashes_raw <- map(raw_files, read_export) |> list_rbind()

# Clean & derive analysis fields
crashes <- crashes_raw |>
  mutate(
    year       = as.integer(year),
    # blank / non-numeric non-motorist counts -> 0
    n_nonmotor = replace_na(suppressWarnings(as.integer(n_nonmotor)), 0L),
    severity   = str_trim(severity),
    group = if_else(n_nonmotor > 0,
                    "Pedestrian/Cyclist/Other",
                    "Driver/Passenger"),
    is_fatal        = severity == "K",
    is_injury_fatal = severity %in% c("A", "K")   # anything not "O"
  ) |>
  # The query date-ranges overlap on their boundary days (2020-01-01, 2022-01-01, 2025-01-01)
  # the same crash can appear in two exports
  # -> de-duplicate on the crash id
  distinct(crash_id, .keep_all = TRUE) |>
  filter(!is.na(year))

# The last export runs to 01/01/2025, so 2025 is just that single boundary day 
# -> keep only complete calendar years
last_complete_year <- 2024L
crashes <- crashes |> filter(year >= 2015L, year <= last_complete_year)

message("Unique crashes after de-duplication: ", nrow(crashes),
        " (", min(crashes$year), "-", max(crashes$year), ")")

# Towns of interest

# Focus on New Haven and Waterbury + comparison cities; plus a statewide total
focus_towns <- c("New Haven", "Waterbury")
comparison_towns <- c("Hartford", "Stamford", "Bridgeport")
report_towns <- c(focus_towns, comparison_towns)

# Per-town, per-year, per-group counts of the three metrics
by_town <- crashes |>
  group_by(geography = town, year, group) |>
  summarise(
    total_crashes          = n(),
    fatal_crashes          = sum(is_fatal),
    injury_or_fatal_crashes = sum(is_injury_fatal),
    .groups = "drop"
  )

# Statewide ("Connecticut") totals: same counts across all towns
by_state <- crashes |>
  group_by(geography = "Connecticut (statewide)", year, group) |>
  summarise(
    total_crashes           = n(),
    fatal_crashes           = sum(is_fatal),
    injury_or_fatal_crashes = sum(is_injury_fatal),
    .groups = "drop"
  )

# Long tidy table: one row per geography x year x group, three metric columns
indicators_long <- bind_rows(by_town, by_state) |>
  filter(geography %in% c(report_towns, "Connecticut (statewide)")) |>
  arrange(geography, group, year)

# Six indicators by year

# Wide layout: for each geography x year, all six indicators as columns (group x metric)
indicators_table <- indicators_long |>
  pivot_wider(
    names_from  = group,
    values_from = c(total_crashes, fatal_crashes, injury_or_fatal_crashes),
    names_glue  = "{group}__{.value}",
    values_fill = 0
  ) |>
  arrange(geography, year)

# Output

dir.create("output", showWarnings = FALSE)
write_csv(indicators_long,  "output/crash_indicators_long.csv")
write_csv(indicators_table, "output/crash_indicators_by_year_wide.csv")

# Readable console view: one compact block per geography, year down the rows
# each of the six indicators as a labelled column
print_geo <- function(geo) {
  message("\n--- ", geo, ": six indicators by year ---")
  indicators_long |>
    filter(geography == geo) |>
    transmute(
      year, group,
      total = total_crashes,
      fatal = fatal_crashes,
      inj_or_fatal = injury_or_fatal_crashes
    ) |>
    pivot_wider(names_from = group,
                values_from = c(total, fatal, inj_or_fatal),
                names_glue = "{substr(group,1,4)}_{.value}") |>
    arrange(year) |>
    print(n = Inf, width = Inf)
}
walk(c("New Haven", "Waterbury", "Connecticut (statewide)"), print_geo)

message("\nWrote:\n  output/crash_indicators_long.csv       (tidy: geography x year x group)",
        "\n  output/crash_indicators_by_year_wide.csv (six indicators by year)")
