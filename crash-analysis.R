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
library(tidycensus)   # Census API access for population denominators

# Compile-once analysis dataset

# The raw csv exports total are too big to uplaod to git
# This compiles them into a small crash_counts_by_town.csv that only includes the 
# three metrics by town x year x road-user group which is committed to git
# Delete crash_counts_by_town.csv (or when the raw files change) to rebuild
compiled_file <- "crash_counts_by_town.csv"

build_town_counts <- function() {
  # Raw exports live in raw_data/ (git-ignored; too big for GitHub) 
  # Each query exports THREE linked tables (_0 crash / _1 vehicle / _2 person)
  # we only need the crash-level _0 file
  # Line 1 is the query URL (skip = 1)
  # files are Windows-1252 encoded, not UTF-8
  # raw_data/exportdescription_*.txt documents each query's date range
  raw_dir <- "raw_data"
  raw_files <- list.files(raw_dir, pattern = "^export_\\d+_0\\.csv$",
                          full.names = TRUE)
  if (length(raw_files) == 0) {
    stop(
      "Compiled file '", compiled_file, "' is missing AND no raw ",
      "'export_*_0.csv' files were found in '", raw_dir, "/' under:\n  ", getwd(),
      "\nRestore ", compiled_file, " (it is committed to git), or place the raw ",
      "exports in raw_data/ and point R at the project folder:\n",
      '  setwd("/Users/alexklee/Desktop/DataHaven/crash-analysis")',
      call. = FALSE
    )
  }
  # The crash file has 107 columns with several repeated names (CrashId appears 3x, etc.)
  # -> select by stable POSITION rather than name:
  #  1 CrashId, 9 Town Name, 14 Year, 23 Crash Severity, 28 Number Of Non-Motorist
  crash_cols <- c(crash_id = 1L, town = 9L, year = 14L,
                  severity = 23L, n_nonmotor = 28L)
  read_export <- function(path) {
    read_csv(path, skip = 1, col_select = all_of(crash_cols),
             col_types = cols(.default = col_character()),
             locale = locale(encoding = "Windows-1252"),
             name_repair = "minimal", show_col_types = FALSE)
  }
  message("Compiling ", length(raw_files), " raw crash files -> ",
          compiled_file, " ...")
  map(raw_files, read_export) |>
    list_rbind() |>
    mutate(
      year       = as.integer(year),
      n_nonmotor = replace_na(suppressWarnings(as.integer(n_nonmotor)), 0L),
      severity   = str_trim(severity),
      group = if_else(n_nonmotor > 0, "Pedestrian/Cyclist/Other",
                      "Driver/Passenger"),
      is_fatal        = severity == "K",
      is_injury_fatal = severity %in% c("A", "K")   # anything not "O"
    ) |>
    # Query date-ranges overlap on boundary days (2020/2022/2025-01-01)
    # so a crash can appear in two exports -> de-duplicate on the crash id
    distinct(crash_id, .keep_all = TRUE) |>
    filter(!is.na(year), year >= 2015L, year <= 2025L) |>
    group_by(town, year, group) |>
    summarise(
      total_crashes           = n(),
      fatal_crashes           = sum(is_fatal),
      injury_or_fatal_crashes = sum(is_injury_fatal),
      .groups = "drop"
    )
}

if (file.exists(compiled_file)) {
  message("Using existing ", compiled_file, " (delete it to rebuild from raw).")
  town_counts <- read_csv(
    compiled_file, show_col_types = FALSE,
    col_types = cols(town = "c", year = "i", group = "c",
                     total_crashes = "i", fatal_crashes = "i",
                     injury_or_fatal_crashes = "i")
  )
} else {
  town_counts <- build_town_counts()
  write_csv(town_counts, compiled_file)
  message("Wrote ", compiled_file, " (", nrow(town_counts), " rows, ",
          min(town_counts$year), "-", max(town_counts$year), ").")
}

# Compile-once fatalities dataset

# Fatal crashes (above) count crashes with >=1 death. 
# This counts total people killed, which can exceed fatal crashes (multi-death crashes)
# Persons killed live in the person-level _2 files
# (Injury Status K), classified by Person Type, joined to the crash _0 files for town/year
# Compiled once to a small fatalities_by_town.csv (committed to git)
fatalities_file <- "fatalities_by_town.csv"

build_fatalities <- function() {
  raw_dir <- "raw_data"
  person_files <- list.files(raw_dir, pattern = "^export_\\d+_2\\.csv$",
                             full.names = TRUE)
  crash_files  <- list.files(raw_dir, pattern = "^export_\\d+_0\\.csv$",
                             full.names = TRUE)
  if (length(person_files) == 0 || length(crash_files) == 0)
    stop("'", fatalities_file, "' is missing and the raw _0/_2 exports were not ",
         "found in raw_data/. Restore the compiled file or the raw data.",
         call. = FALSE)

  read_raw <- function(path, sel)
    read_csv(path, skip = 1, col_select = all_of(sel),
             col_types = cols(.default = col_character()),
             locale = locale(encoding = "Windows-1252"),
             name_repair = "minimal", show_col_types = FALSE)

  # CrashId -> town, year (dedup: same crash appears across boundary exports)
  message("Building fatalities: reading crash files for town/year ...")
  crash_map <- map(crash_files, ~read_raw(.x, c(crash_id = 1L, town = 9L,
                                                year = 14L))) |>
    list_rbind() |>
    mutate(year = as.integer(year)) |>
    distinct(crash_id, .keep_all = TRUE)

  # Persons killed (Injury Status K), classified by Person Type
  # Motorists = driver/passenger/occupant (1,2,7); non-motorists = ped/cyclist/other (3,4,5,6,8); 
  # anything else (witness/unknown) kept as Other so it still counts toward the all-road-users total
  motorist <- c("1", "2", "7")
  nonmotor <- c("3", "4", "5", "6", "8")
  message("Building fatalities: reading person files (Injury Status K) ...")
  map(person_files, ~read_raw(.x, c(crash_id = 1L, person_id = 3L,
                                    ptype = 9L, injury = 11L))) |>
    list_rbind() |>
    filter(injury == "K") |>
    distinct(crash_id, person_id, .keep_all = TRUE) |>   # dedup boundary overlaps
    mutate(group = case_when(ptype %in% motorist ~ "Driver/Passenger",
                             ptype %in% nonmotor ~ "Pedestrian/Cyclist/Other",
                             TRUE               ~ "Other/Unknown")) |>
    inner_join(crash_map, by = "crash_id") |>
    filter(!is.na(year), year >= 2015L, year <= 2025L) |>
    group_by(town, year, group) |>
    summarise(deaths = n(), .groups = "drop")
}

if (file.exists(fatalities_file)) {
  message("Using existing ", fatalities_file, " (delete it to rebuild from raw).")
  fatal_counts <- read_csv(fatalities_file, show_col_types = FALSE,
    col_types = cols(town = "c", year = "i", group = "c", deaths = "i"))
} else {
  fatal_counts <- build_fatalities()
  write_csv(fatal_counts, fatalities_file)
  message("Wrote ", fatalities_file, " (", nrow(fatal_counts), " rows, ",
          sum(fatal_counts$deaths), " deaths ",
          min(fatal_counts$year), "-", max(fatal_counts$year), ").")
}

# Compile-once by-COLLISION-TYPE datasets

# Annual calculations by collision type. 
# Each crash is from the people involved (person-level _2 files):
#   Cyclist/vehicle    -> a cyclist was involved (Person Type 5,6)
#   Pedestrian/vehicle -> a pedestrian / other non-motorist involved (3,4,8), no cyclist
#   Vehicle/vehicle    -> motor-vehicle occupants only (includes single-vehicle crashes)
# Produces crashes and fatalities by town x year x collision type
bytype_crash_file <- "crash_counts_by_town_bytype.csv"
bytype_fatal_file <- "fatalities_by_town_bytype.csv"

build_bytype <- function() {
  raw_dir <- "raw_data"
  person_files <- list.files(raw_dir, "^export_\\d+_2\\.csv$", full.names = TRUE)
  crash_files  <- list.files(raw_dir, "^export_\\d+_0\\.csv$", full.names = TRUE)
  if (length(person_files) == 0 || length(crash_files) == 0)
    stop("By-type compiled files are missing and raw _0/_2 exports were not ",
         "found in raw_data/. Restore the compiled files or the raw data.",
         call. = FALSE)
  read_raw <- function(path, sel)
    read_csv(path, skip = 1, col_select = all_of(sel),
             col_types = cols(.default = col_character()),
             locale = locale(encoding = "Windows-1252"),
             name_repair = "minimal", show_col_types = FALSE)

  message("Building by-type: reading person files ...")
  persons <- map(person_files, ~read_raw(.x, c(crash_id = 1L, person_id = 3L,
                                               ptype = 9L, injury = 11L))) |>
    list_rbind() |>
    distinct(crash_id, person_id, .keep_all = TRUE)

  # One collision type per crash, from the people involved
  cyc <- c("5", "6"); ped <- c("3", "4", "8")
  crash_type <- persons |>
    group_by(crash_id) |>
    summarise(has_cyc = any(ptype %in% cyc),
              has_ped = any(ptype %in% ped), .groups = "drop") |>
    mutate(crash_type = case_when(has_cyc ~ "Cyclist/vehicle",
                                  has_ped ~ "Pedestrian/vehicle",
                                  TRUE    ~ "Vehicle/vehicle")) |>
    select(crash_id, crash_type)

  message("Building by-type: reading crash files ...")
  crashes <- map(crash_files, ~read_raw(.x, c(crash_id = 1L, town = 9L,
                                              year = 14L, severity = 23L))) |>
    list_rbind() |>
    mutate(year = as.integer(year), severity = str_trim(severity)) |>
    distinct(crash_id, .keep_all = TRUE) |>
    filter(!is.na(year), year >= 2015L, year <= 2025L) |>
    left_join(crash_type, by = "crash_id") |>
    mutate(crash_type = replace_na(crash_type, "Vehicle/vehicle"))

  crash_counts <- crashes |>
    group_by(town, year, crash_type) |>
    summarise(total_crashes           = n(),
              fatal_crashes           = sum(severity == "K"),
              injury_or_fatal_crashes = sum(severity %in% c("A", "K")),
              .groups = "drop")

  # Fatalities tagged with their crash's collision type
  deaths <- persons |>
    filter(injury == "K") |>
    left_join(crash_type, by = "crash_id") |>
    mutate(crash_type = replace_na(crash_type, "Vehicle/vehicle")) |>
    inner_join(select(crashes, crash_id, town, year), by = "crash_id") |>
    group_by(town, year, crash_type) |>
    summarise(deaths = n(), .groups = "drop")

  list(crashes = crash_counts, deaths = deaths)
}

if (file.exists(bytype_crash_file) && file.exists(bytype_fatal_file)) {
  message("Using existing by-type compiled files (delete to rebuild from raw).")
  bytype_crashes <- read_csv(bytype_crash_file, show_col_types = FALSE,
    col_types = cols(town = "c", year = "i", crash_type = "c",
                     total_crashes = "i", fatal_crashes = "i",
                     injury_or_fatal_crashes = "i"))
  bytype_deaths <- read_csv(bytype_fatal_file, show_col_types = FALSE,
    col_types = cols(town = "c", year = "i", crash_type = "c", deaths = "i"))
} else {
  bt <- build_bytype()
  bytype_crashes <- bt$crashes
  bytype_deaths  <- bt$deaths
  write_csv(bytype_crashes, bytype_crash_file)
  write_csv(bytype_deaths,  bytype_fatal_file)
  message("Wrote by-type files (", sum(bytype_deaths$deaths), " deaths).")
}

# Geographies of interest

# Focus on New Haven and Waterbury + comparison cities
# report_towns are also the five most populous CT cities
# Two aggregate benchmark lines are added: the five largest cities combined, and the statewide total
focus_towns <- c("New Haven", "Waterbury")
comparison_towns <- c("Hartford", "Stamford", "Bridgeport")
report_towns <- c(focus_towns, comparison_towns)   # = the 5 largest CT cities
big5_label   <- "Five largest cities"
state_label  <- "Connecticut (statewide)"
aggregates   <- c(big5_label, state_label)          # emphasized benchmark lines

# Per-town counts are already at town x year x group grain
by_town <- town_counts |> rename(geography = town)

# Statewide total: sum every town within each year x group
by_state <- town_counts |>
  group_by(geography = state_label, year, group) |>
  summarise(across(ends_with("_crashes"), sum), .groups = "drop")

# Five largest cities combined: sum just the report towns
by_big5 <- town_counts |>
  filter(town %in% report_towns) |>
  group_by(geography = big5_label, year, group) |>
  summarise(across(ends_with("_crashes"), sum), .groups = "drop")

# Long tidy table: one row per geography x year x group, three metric columns
indicators_long <- bind_rows(by_town, by_state, by_big5) |>
  filter(geography %in% c(report_towns, aggregates)) |>
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

# Readable console view: one compact block per geography, year down the rows each of the six indicators as a labelled column
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
walk(c("New Haven", "Waterbury", big5_label, state_label), print_geo)

# Per-capita rates + time-series graph

# single fixed denominator per geography from the 2020 Decennial Census count
fetch_census_population <- function() {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (!nzchar(key))
    stop("CENSUS_API_KEY is not set. Per-capita rates require Census API ",
         "access; add 'CENSUS_API_KEY=...' to ~/.Renviron.", call. = FALSE)

  # Place GEOIDs (state 09 + 5-digit place code) for the five cities
  places <- tribble(
    ~geography,   ~GEOID,
    "New Haven",  "0952000",
    "Waterbury",  "0980000",
    "Hartford",   "0937000",
    "Stamford",   "0973000",
    "Bridgeport", "0908000"
  )
  message("Fetching 2020 Decennial Census populations via tidycensus ...")
  city <- get_decennial("place", variables = "P1_001N", state = "CT",
                        year = 2020, sumfile = "pl", key = key) |>
    inner_join(places, by = "GEOID") |>
    transmute(geography, population = value)
  state <- get_decennial("state", variables = "P1_001N", state = "CT",
                         year = 2020, sumfile = "pl", key = key) |>
    transmute(geography = "Connecticut (statewide)", population = value)
  bind_rows(city, state)
}

population <- fetch_census_population()

# Combined denominator for the "Five largest cities" line = sum of their 2020 populations
population <- population |>
  bind_rows(
    population |> filter(geography %in% report_towns) |>
      summarise(population = sum(population)) |>
      mutate(geography = big5_label)
  )

metric_labels <- c(
  total_crashes           = "Total crashes",
  injury_or_fatal_crashes = "Injury or fatal crashes",
  fatal_crashes           = "Fatal crashes"
)

rates_long <- indicators_long |>
  left_join(population, by = "geography") |>
  pivot_longer(
    cols      = c(total_crashes, injury_or_fatal_crashes, fatal_crashes),
    names_to  = "metric", values_to = "count"
  ) |>
  mutate(
    rate_per_100k = count / population * 1e5,
    metric = factor(metric, levels = names(metric_labels),
                    labels = metric_labels)
  )

write_csv(rates_long, "output/crash_rates_per_100k.csv")

# Plot only years that have a denominator
rate_data <- rates_long |> filter(!is.na(rate_per_100k))
rate_span <- range(rate_data$year)

# One panel per group x metric, laid out as 2 rows (group) x 3 cols (metric)
# facet_wrap gives every panel its own y-scale, so the small fatal-crash rates
# stay legible instead of being flattened by the much larger total-crash rates
rate_plot <- rate_data |>
  mutate(kind = if_else(geography %in% aggregates, "aggregate", "city")) |>
  ggplot(aes(year, rate_per_100k, colour = geography)) +
  geom_line(aes(linewidth = kind)) +
  geom_point(size = 1.1) +
  facet_wrap(vars(group, metric), scales = "free_y", nrow = 2,
             labeller = labeller(.multi_line = FALSE)) +
  scale_x_continuous(breaks = seq(rate_span[1], rate_span[2], 2)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_linewidth_manual(values = c(city = 0.6, aggregate = 1.4), guide = "none") +
  labs(
    title    = sprintf("Connecticut crash indicators per 100,000 residents, %d-%d",
                       rate_span[1], rate_span[2]),
    subtitle = "By road-user group; individual cities (thin) vs. five-largest-cities and statewide benchmarks (thick)",
    x = NULL, y = "Crashes per 100,000 residents", colour = NULL,
    caption  = "Sources: CT Crash Data Repository (ctcrash.uconn.edu); population from the 2020 U.S. Decennial Census (held constant across all years)."
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/crash_rates_per_100k.png", rate_plot,
       width = 11, height = 6.5, dpi = 300, device = ragg::agg_png)

# Draw it to the Plots pane too (in addition to the saved PNG)
print(rate_plot)

# Fatal-crash share + graph

# Share of crashes that are fatal (fatal / total, as a %)
# Same geographic breakdown as the rates graph, faceted by road-user group plus an "All road users" panel
fatal_share <- indicators_long |>
  bind_rows(
    indicators_long |>
      group_by(geography, year) |>
      summarise(across(ends_with("_crashes"), sum), .groups = "drop") |>
      mutate(group = "All road users")
  ) |>
  mutate(
    fatal_pct = fatal_crashes / total_crashes * 100,
    group = factor(group, levels = c("All road users", "Driver/Passenger",
                                     "Pedestrian/Cyclist/Other"))
  ) |>
  arrange(geography, group, year)

write_csv(
  fatal_share |> select(geography, year, group, total_crashes, fatal_crashes,
                        fatal_pct),
  "output/fatal_share.csv"
)

fatal_span <- range(fatal_share$year)
fatal_plot <- fatal_share |>
  mutate(kind = if_else(geography %in% aggregates, "aggregate", "city")) |>
  ggplot(aes(year, fatal_pct, colour = geography)) +
  geom_line(aes(linewidth = kind)) +
  geom_point(size = 1.1) +
  facet_wrap(vars(group), scales = "free_y", nrow = 1) +
  scale_x_continuous(breaks = seq(fatal_span[1], fatal_span[2], 2)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_linewidth_manual(values = c(city = 0.6, aggregate = 1.4), guide = "none") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    title    = sprintf("Share of Connecticut crashes that are fatal, %d-%d",
                       fatal_span[1], fatal_span[2]),
    subtitle = "Fatal crashes as a percent of all crashes; individual cities (thin) vs. five-largest-cities and statewide benchmarks (thick)",
    x = NULL, y = "Fatal crashes (% of crashes)", colour = NULL,
    caption  = "Source: CT Crash Data Repository (ctcrash.uconn.edu)."
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fatal_share.png", fatal_plot,
       width = 11, height = 4.8, dpi = 300, device = ragg::agg_png)
print(fatal_plot)

# Fatalities (persons killed) per capita + graph

# total fatalities: people killed per 100,000 residents from the person-level fatalities_by_town.csv
# Same geographies + benchmark lines as the rates graph
# faceted by victim type plus "All road users" (= every person killed)
# Uses the same fixed 2020 Decennial population denominators
fatal_geo <- bind_rows(
  fatal_counts |> rename(geography = town),
  fatal_counts |> group_by(geography = state_label, year, group) |>
    summarise(deaths = sum(deaths), .groups = "drop"),
  fatal_counts |> filter(town %in% report_towns) |>
    group_by(geography = big5_label, year, group) |>
    summarise(deaths = sum(deaths), .groups = "drop")
) |>
  filter(geography %in% c(report_towns, aggregates))

# All road users = every person killed (incl. Other/Unknown victim types)
deaths_display <- bind_rows(
  fatal_geo,
  fatal_geo |> group_by(geography, year) |>
    summarise(deaths = sum(deaths), group = "All road users", .groups = "drop")
) |>
  filter(group %in% c("All road users", "Driver/Passenger",
                      "Pedestrian/Cyclist/Other")) |>
  # Years with zero deaths in a cell are absent -> fill 0 so lines don't gap
  complete(nesting(geography), year = 2015:2025,
           group = c("All road users", "Driver/Passenger",
                     "Pedestrian/Cyclist/Other"),
           fill = list(deaths = 0)) |>
  left_join(population, by = "geography") |>
  mutate(
    deaths_per_100k = deaths / population * 1e5,
    group = factor(group, levels = c("All road users", "Driver/Passenger",
                                     "Pedestrian/Cyclist/Other"))
  )

write_csv(
  deaths_display |> select(geography, year, group, deaths, population,
                           deaths_per_100k),
  "output/fatalities_per_100k.csv"
)

deaths_plotdata <- deaths_display |> filter(!is.na(deaths_per_100k))
deaths_span <- range(deaths_plotdata$year)
deaths_plot <- deaths_plotdata |>
  mutate(kind = if_else(geography %in% aggregates, "aggregate", "city")) |>
  ggplot(aes(year, deaths_per_100k, colour = geography)) +
  geom_line(aes(linewidth = kind)) +
  geom_point(size = 1.1) +
  facet_wrap(vars(group), scales = "free_y", nrow = 1) +
  scale_x_continuous(breaks = seq(deaths_span[1], deaths_span[2], 2)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_linewidth_manual(values = c(city = 0.6, aggregate = 1.4), guide = "none") +
  labs(
    title    = sprintf("Connecticut traffic fatalities per 100,000 residents, %d-%d",
                       deaths_span[1], deaths_span[2]),
    subtitle = "Persons killed (not fatal crashes), by victim type; individual cities (thin) vs. five-largest-cities and statewide benchmarks (thick)",
    x = NULL, y = "Persons killed per 100,000 residents", colour = NULL,
    caption  = "Source: CT Crash Data Repository (ctcrash.uconn.edu). Population: 2020 U.S. Decennial Census (held constant across all years)."
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fatalities_per_100k.png", deaths_plot,
       width = 11, height = 4.8, dpi = 300, device = ragg::agg_png)
print(deaths_plot)

# By-collision-type annual graphs (vehicle/vehicle, pedestrian/vehicle, cyclist/vehicle)

type_levels <- c("Vehicle/vehicle", "Pedestrian/vehicle", "Cyclist/vehicle")

# Roll town-level by-type counts up to the reporting geographies (+ benchmarks),
# fill missing town-year-type cells with 0, and attach the fixed 2020 population
roll_bytype <- function(df, value_cols) {
  bind_rows(
    df |> rename(geography = town),
    df |> group_by(geography = state_label, year, crash_type) |>
      summarise(across(all_of(value_cols), sum), .groups = "drop"),
    df |> filter(town %in% report_towns) |>
      group_by(geography = big5_label, year, crash_type) |>
      summarise(across(all_of(value_cols), sum), .groups = "drop")
  ) |>
    filter(geography %in% c(report_towns, aggregates)) |>
    complete(geography, year = 2015:2025, crash_type = type_levels,
             fill = setNames(as.list(rep(0, length(value_cols))), value_cols)) |>
    left_join(population, by = "geography") |>
    mutate(crash_type = factor(crash_type, levels = type_levels))
}

# Shared plotter: one facet per collision type, per-100k rate, benchmarks thick
bytype_plot <- function(df, value, y_lab, title) {
  df |>
    mutate(rate = .data[[value]] / population * 1e5,
           kind = if_else(geography %in% aggregates, "aggregate", "city")) |>
    ggplot(aes(year, rate, colour = geography)) +
    geom_line(aes(linewidth = kind)) +
    geom_point(size = 1.1) +
    facet_wrap(vars(crash_type), scales = "free_y", nrow = 1) +
    scale_x_continuous(breaks = seq(2015, 2025, 2)) +
    scale_colour_brewer(palette = "Dark2") +
    scale_linewidth_manual(values = c(city = 0.6, aggregate = 1.4), guide = "none") +
    labs(
      title    = title,
      subtitle = "By collision type; individual cities (thin) vs. five-largest-cities and statewide benchmarks (thick)",
      x = NULL, y = y_lab, colour = NULL,
      caption  = "Source: CT Crash Data Repository (ctcrash.uconn.edu). Population: 2020 U.S. Decennial Census (held constant). 'Vehicle/vehicle' includes single-vehicle crashes."
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"))
}

crashes_bytype_geo <- roll_bytype(bytype_crashes,
  c("total_crashes", "fatal_crashes", "injury_or_fatal_crashes"))
deaths_bytype_geo <- roll_bytype(bytype_deaths, "deaths")

write_csv(
  crashes_bytype_geo |>
    mutate(across(ends_with("_crashes"), ~ .x / population * 1e5,
                  .names = "{.col}_per_100k")),
  "output/crashes_by_type.csv"
)
write_csv(
  deaths_bytype_geo |> mutate(deaths_per_100k = deaths / population * 1e5),
  "output/fatalities_by_type.csv"
)

crashes_type_plot <- bytype_plot(crashes_bytype_geo, "total_crashes",
  "Crashes per 100,000 residents",
  "Connecticut crashes per 100,000 residents by collision type, 2015-2025")
ggsave("output/crashes_by_type.png", crashes_type_plot,
       width = 11, height = 4.8, dpi = 300, device = ragg::agg_png)
print(crashes_type_plot)

deaths_type_plot <- bytype_plot(deaths_bytype_geo, "deaths",
  "Persons killed per 100,000 residents",
  "Connecticut traffic fatalities per 100,000 residents by collision type, 2015-2025")
ggsave("output/fatalities_by_type.png", deaths_type_plot,
       width = 11, height = 4.8, dpi = 300, device = ragg::agg_png)
print(deaths_type_plot)

# Fatal share by collision type: what % of each type's crashes are fatal
fatal_share_bytype <- crashes_bytype_geo |>
  mutate(fatal_pct = if_else(total_crashes > 0,
                             fatal_crashes / total_crashes * 100, NA_real_))

write_csv(
  fatal_share_bytype |> select(geography, year, crash_type, total_crashes,
                               fatal_crashes, fatal_pct),
  "output/fatal_share_by_type.csv"
)

fsbt_plot <- fatal_share_bytype |>
  filter(!is.na(fatal_pct)) |>
  mutate(kind = if_else(geography %in% aggregates, "aggregate", "city")) |>
  ggplot(aes(year, fatal_pct, colour = geography)) +
  geom_line(aes(linewidth = kind)) +
  geom_point(size = 1.1) +
  facet_wrap(vars(crash_type), scales = "free_y", nrow = 1) +
  scale_x_continuous(breaks = seq(2015, 2025, 2)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_linewidth_manual(values = c(city = 0.6, aggregate = 1.4), guide = "none") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  labs(
    title    = "Share of Connecticut crashes that are fatal, by collision type, 2015-2025",
    subtitle = "Fatal crashes as a percent of that type's crashes; individual cities (thin) vs. five-largest-cities and statewide benchmarks (thick)",
    x = NULL, y = "Fatal crashes (% of that type's crashes)", colour = NULL,
    caption  = "Source: CT Crash Data Repository (ctcrash.uconn.edu). 'Vehicle/vehicle' includes single-vehicle crashes."
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/fatal_share_by_type.png", fsbt_plot,
       width = 11, height = 4.8, dpi = 300, device = ragg::agg_png)
print(fsbt_plot)

message("\nWrote:\n  output/crash_indicators_long.csv       (tidy: geography x year x group)",
        "\n  output/crash_indicators_by_year_wide.csv (six indicators by year)",
        "\n  output/crash_rates_per_100k.csv          (per-capita rates, tidy)",
        "\n  output/crash_rates_per_100k.png          (per-capita time-series graph)",
        "\n  output/fatal_share.csv                   (fatal % of crashes, tidy)",
        "\n  output/fatal_share.png                   (fatal-share time-series graph)",
        "\n  output/fatalities_per_100k.csv           (persons killed per 100k, tidy)",
        "\n  output/fatalities_per_100k.png           (fatalities per-capita graph)",
        "\n  output/crashes_by_type.csv / .png        (crashes by collision type)",
        "\n  output/fatalities_by_type.csv / .png     (fatalities by collision type)",
        "\n  output/fatal_share_by_type.csv / .png    (fatal % of crashes by collision type)")
