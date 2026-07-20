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
library(jsonlite)   # live Census API calls for population denominators

# Compile-once analysis dataset

# The raw csv exports total are too big to uplaod to git
# This compiles them into a small crash_counts_by_town.csv that only includes the 
# three metrics by town x year x road-user group which is committed to git
# Delete crash_counts_by_town.csv (or when the raw files change) to rebuild
compiled_file <- "crash_counts_by_town.csv"

build_town_counts <- function() {
  # Each query exports THREE linked tables (_0 crash / _1 vehicle / _2 person);
  # we only need the crash-level _0 file. Line 1 is the query URL (skip = 1);
  # files are Windows-1252 encoded (curly quotes), not UTF-8.
  raw_files <- list.files(pattern = "^export_\\d+_0\\.csv$")
  if (length(raw_files) == 0) {
    stop(
      "Compiled file '", compiled_file, "' is missing AND no raw ",
      "'export_*_0.csv' files were found in:\n  ", getwd(),
      "\nRestore ", compiled_file, " (it is committed to git), or point R at ",
      "the folder holding the raw exports:\n",
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
    # Query date-ranges overlap on boundary days (2020/2022/2025-01-01), so a
    # crash can appear in two exports -> de-duplicate on the crash id
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

# Geographies of interest

# Focus on New Haven and Waterbury + comparison cities; plus a statewide total
focus_towns <- c("New Haven", "Waterbury")
comparison_towns <- c("Hartford", "Stamford", "Bridgeport")
report_towns <- c(focus_towns, comparison_towns)

# Per-town counts are already at town x year x group grain.
by_town <- town_counts |> rename(geography = town)

# Statewide ("Connecticut") totals: sum every town within each year x group.
by_state <- town_counts |>
  group_by(geography = "Connecticut (statewide)", year, group) |>
  summarise(across(ends_with("_crashes"), sum), .groups = "drop")

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

# Per-capita rates + time-series graph

# Population denominators are fetched from Census API each run
#   2015-2019, 2021-2024 : ACS 1-year estimates (B01003_001E)
#   2020                 : 2020 Decennial Census (P1_001N); ACS 1-yr not released
# 2025 has no published figure yet; it is handled just below by carrying the
# 2024 value forward (see that block).
# Requires CENSUS_API_KEY in the environment (e.g. ~/.Renviron).
fetch_census_population <- function() {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (!nzchar(key))
    stop("CENSUS_API_KEY is not set. Per-capita rates require live Census API ",
         "access; add 'CENSUS_API_KEY=...' to ~/.Renviron.", call. = FALSE)

  places <- tribble(
    ~geography,   ~place,
    "New Haven",  "52000",
    "Waterbury",  "80000",
    "Hartford",   "37000",
    "Stamford",   "73000",
    "Bridgeport", "08000"
  )
  place_list <- str_c(places$place, collapse = ",")

  census_get <- function(url) {
    raw <- jsonlite::fromJSON(url)
    as_tibble(raw[-1, , drop = FALSE], .name_repair = "minimal") |>
      set_names(raw[1, ])
  }
  # Cities + statewide total for one dataset/variable/year.
  fetch_year <- function(year, dataset, var) {
    base <- sprintf("https://api.census.gov/data/%d/%s", year, dataset)
    city <- census_get(sprintf("%s?get=NAME,%s&for=place:%s&in=state:09&key=%s",
                               base, var, place_list, key)) |>
      transmute(place = place, population = as.numeric(.data[[var]])) |>
      left_join(places, by = "place") |>
      select(geography, population)
    state <- census_get(sprintf("%s?get=NAME,%s&for=state:09&key=%s",
                                base, var, key)) |>
      transmute(geography = "Connecticut (statewide)",
                population = as.numeric(.data[[var]]))
    bind_rows(city, state) |> mutate(year = year)
  }

  message("Fetching Census populations (ACS 1-year + 2020 Decennial) ...")
  acs <- map(c(2015:2019, 2021:2024),
             ~fetch_year(.x, "acs/acs1", "B01003_001E")) |> list_rbind()
  dec2020 <- fetch_year(2020, "dec/pl", "P1_001N")
  bind_rows(acs, dec2020) |> arrange(geography, year)
}

population <- fetch_census_population()

# 2025 has no published Census figure yet (ACS 1-year 2025 releases late 2026).
# Deliberate choice: carry the 2024 ACS value forward as the 2025 denominator so
# the per-capita graph extends through 2025. This is the ONLY non-published
# population figure here; population barely moves year-to-year, so 2025 rates are
# a close approximation. Delete this block to revert to omitting 2025, or replace
# with the real 2025 ACS/PEP figure once it is available.
population <- population |>
  bind_rows(population |> filter(year == 2024L) |> mutate(year = 2025L))

metric_labels <- c(
  total_crashes           = "Total crashes",
  injury_or_fatal_crashes = "Injury or fatal crashes",
  fatal_crashes           = "Fatal crashes"
)

rates_long <- indicators_long |>
  left_join(population, by = c("geography", "year")) |>
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

# One panel per group x metric, laid out as 2 rows (group) x 3 cols (metric).
# facet_wrap gives every panel its OWN y-scale, so the small fatal-crash rates
# stay legible instead of being flattened by the much larger total-crash rates.
rate_plot <- rate_data |>
  ggplot(aes(year, rate_per_100k, colour = geography)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.1) +
  facet_wrap(vars(group, metric), scales = "free_y", nrow = 2,
             labeller = labeller(.multi_line = FALSE)) +
  scale_x_continuous(breaks = seq(rate_span[1], rate_span[2], 2)) +
  scale_colour_brewer(palette = "Dark2") +
  labs(
    title    = sprintf("Connecticut crash indicators per 100,000 residents, %d-%d",
                       rate_span[1], rate_span[2]),
    subtitle = "By road-user group; New Haven & Waterbury vs. comparison cities and the state",
    x = NULL, y = "Crashes per 100,000 residents", colour = NULL,
    caption  = "Sources: CT Crash Data Repository (ctcrash.uconn.edu); population from U.S. Census ACS 1-year estimates (2020: Decennial Census; 2025: 2024 ACS carried forward)."
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

ggsave("output/crash_rates_per_100k.png", rate_plot,
       width = 11, height = 6.5, dpi = 150)

message("\nWrote:\n  output/crash_indicators_long.csv       (tidy: geography x year x group)",
        "\n  output/crash_indicators_by_year_wide.csv (six indicators by year)",
        "\n  output/crash_rates_per_100k.csv          (per-capita rates, tidy)",
        "\n  output/crash_rates_per_100k.png          (per-capita time-series graph)")
