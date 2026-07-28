# New Haven vs. Connecticut crash trends back to 2010

# Reads:  raw_data/conndot/export_*_{0,1}.csv   (2010-2014, git-ignored)
#         crash_counts_by_town_bytype.csv       (2015-2025, committed)
# Writes: crash_counts_by_town_bytype_conndot.csv  (compiled once, committed)
#         output/pre2015_check_by_mode.csv
#         output/pre2015_check_rates.png

# READ THIS BEFORE USING THE PRE-2015 NUMBERS

# 1. The CT Crash Data Repository serves two datasets: "conndot" (through 2014) and "mmucc" (2015 on).
#    The crash form and the reporting thresholds both changed. 

# 2. 2011 is incomplete. Statewide it has 78,435 crashes against ~95-102k in the years either side. 
#    January and February look normal (10.8k / 9.2k) and then March drops to 4.6k and never recovers, 
#    while 2010 and 2012-2014 hold ~7-9.4k every month. 
#    Injury and fatal counts for 2011 are in line with neighbouring years, so what went missing is property-damage-only reporting.

library(tidyverse)
library(tidycensus)
library(patchwork)

conndot_years <- 2010:2014   # complete years covered by the ConnDOT exports
mmucc_years   <- 2015:2025
break_year    <- 2014.5      # drawn between the two datasets

# 1. Compile the ConnDOT exports (2010-2014)

# The ConnDOT export has a different setup from the MMUCC
# 59 crash columns instead of 103, and no person-type field
# Columns are selected by position, as in crash-analysis.R:

#   crash   _0 : 1 Crash ID, 3 Date Of Crash (MM/DD/YYYY), 6 Severity Text, 13 Town Text
#   vehicle _1 : 1 Crash ID, 7 Traffic Unit Type Text, 11 Vehicle Type Text

# Mode comes from the vehicle file rather than the person file: 
# a traffic unit of type "Pedestrian" makes the crash pedestrian, a Vehicle Type of "Pedalcycle" makes it cyclist. 
# Mopeds and motorscooters stay motorists, which matches the MMUCC person-type coding.

# Severity Text maps 1:1 onto the three-level MMUCC Crash Severity:
#   "Fatality" -> K, "Injury (No fatality)" -> A, "Property Damage Only" -> O

conndot_file <- "crash_counts_by_town_bytype_conndot.csv"

bytype_cols <- cols(town = "c", year = "i", crash_type = "c",
                    total_crashes = "i", fatal_crashes = "i",
                    injury_or_fatal_crashes = "i")

build_bytype_conndot <- function() {
  raw_dir <- file.path("raw_data", "conndot")
  crash_files   <- list.files(raw_dir, "^export_\\d+_0\\.csv$", full.names = TRUE)
  vehicle_files <- list.files(raw_dir, "^export_\\d+_1\\.csv$", full.names = TRUE)
  if (length(crash_files) == 0 || length(vehicle_files) == 0)
    stop("'", conndot_file, "' is missing and no ConnDOT exports were found in ",
         raw_dir, "/. Restore the compiled file or the raw exports.",
         call. = FALSE)

  read_raw <- function(path, sel)
    read_csv(path, skip = 1, col_select = all_of(sel),
             col_types = cols(.default = col_character()),
             locale = locale(encoding = "Windows-1252"),
             name_repair = "minimal", show_col_types = FALSE)

  message("Compiling ConnDOT: reading ", length(vehicle_files),
          " vehicle files ...")
  crash_type <- map(vehicle_files,
                    ~read_raw(.x, c(crash_id = 1L, unit_type = 7L,
                                    vehicle_type = 11L))) |>
    list_rbind() |>
    group_by(crash_id) |>
    summarise(has_cyc = any(vehicle_type == "Pedalcycle", na.rm = TRUE),
              has_ped = any(unit_type == "Pedestrian", na.rm = TRUE),
              .groups = "drop") |>
    mutate(crash_type = case_when(has_cyc ~ "Cyclist/vehicle",
                                  has_ped ~ "Pedestrian/vehicle",
                                  TRUE    ~ "Vehicle/vehicle")) |>
    select(crash_id, crash_type)

  message("Compiling ConnDOT: reading ", length(crash_files),
          " crash files ...")
  map(crash_files, ~read_raw(.x, c(crash_id = 1L, date = 3L, severity = 6L,
                                   town = 13L))) |>
    list_rbind() |>
    # Most query ranges share their boundary day (01/01) with the next export, so a crash can appear twice
    distinct(crash_id, .keep_all = TRUE) |>
    mutate(year = as.integer(str_sub(date, 7L, 10L))) |>
    # Ranges that end on 01/01 would otherwise pull in a one-day sliver of the following year
    filter(!is.na(town), year %in% conndot_years) |>
    left_join(crash_type, by = "crash_id") |>
    mutate(crash_type = replace_na(crash_type, "Vehicle/vehicle")) |>
    group_by(town, year, crash_type) |>
    summarise(
      total_crashes           = n(),
      fatal_crashes           = sum(severity == "Fatality", na.rm = TRUE),
      injury_or_fatal_crashes = sum(severity %in% c("Fatality",
                                                    "Injury (No fatality)"),
                                    na.rm = TRUE),
      .groups = "drop"
    )
}

if (file.exists(conndot_file)) {
  message("Using existing ", conndot_file, " (delete it to rebuild from raw).")
  bytype_conndot <- read_csv(conndot_file, show_col_types = FALSE,
                             col_types = bytype_cols)
} else {
  bytype_conndot <- build_bytype_conndot()
  write_csv(bytype_conndot, conndot_file)
  message("Wrote ", conndot_file, " (", nrow(bytype_conndot), " rows, ",
          min(bytype_conndot$year), "-", max(bytype_conndot$year), ").")
}

# 2. Join to the 2015-2025 MMUCC counts

bytype_crash_file <- "crash_counts_by_town_bytype.csv"
if (!file.exists(bytype_crash_file))
  stop("'", bytype_crash_file, "' not found. Run crash-analysis.R first to ",
       "compile it from raw_data/.", call. = FALSE)

bytype_mmucc <- read_csv(bytype_crash_file, show_col_types = FALSE,
                         col_types = bytype_cols)

bytype_all <- bind_rows(bytype_conndot, bytype_mmucc)

split_towns <- with(bytype_all,
                    intersect(town, str_remove_all(town[str_detect(town, "\\s")],
                                                   "\\s+")))
if (length(split_towns) > 0)
  stop("Compiled crash counts still contain split town spellings (",
       paste(head(split_towns, 5), collapse = ", "),
       if (length(split_towns) > 5) ", ...", "). Delete ", bytype_crash_file,
       " and re-run crash-analysis.R to rebuild it clean.", call. = FALSE)

nh_label    <- "New Haven"
state_label <- "Connecticut (statewide)"
geo_levels  <- c(nh_label, state_label)

mode_levels <- c("Cars", "Pedestrians", "Bicyclists")
mode_from_type <- c("Vehicle/vehicle"    = "Cars",
                    "Pedestrian/vehicle" = "Pedestrians",
                    "Cyclist/vehicle"    = "Bicyclists")

all_years <- c(conndot_years, mmucc_years)

counts <- bind_rows(
  bytype_all |> filter(town == nh_label) |>
    group_by(year, crash_type) |>
    summarise(across(ends_with("_crashes"), sum), .groups = "drop") |>
    mutate(geography = nh_label),
  bytype_all |> group_by(year, crash_type) |>
    summarise(across(ends_with("_crashes"), sum), .groups = "drop") |>
    mutate(geography = state_label)
) |>
  mutate(mode = factor(unname(mode_from_type[crash_type]), levels = mode_levels)) |>
  select(geography, year, mode, total_crashes, fatal_crashes,
         injury_or_fatal_crashes) |>
  complete(geography, year = all_years, mode,
           fill = list(total_crashes = 0L, fatal_crashes = 0L,
                       injury_or_fatal_crashes = 0L)) |>
  mutate(geography = factor(geography, levels = geo_levels),
         dataset   = if_else(year %in% conndot_years, "ConnDOT", "MMUCC"))

# 3. Per-capita rates, on the same fixed 2020 denominators used elsewhere

fetch_population <- function() {
  key <- Sys.getenv("CENSUS_API_KEY")
  if (!nzchar(key))
    stop("CENSUS_API_KEY is not set. Per-capita rates require Census API ",
         "access; add 'CENSUS_API_KEY=...' to ~/.Renviron.", call. = FALSE)
  message("Fetching 2020 Decennial Census populations via tidycensus ...")
  bind_rows(
    get_decennial("place", variables = "P1_001N", state = "CT",
                  year = 2020, sumfile = "pl", key = key) |>
      filter(GEOID == "0952000") |>          # New Haven city
      transmute(geography = nh_label, population = value),
    get_decennial("state", variables = "P1_001N", state = "CT",
                  year = 2020, sumfile = "pl", key = key) |>
      transmute(geography = state_label, population = value)
  )
}

indicators <- counts |>
  left_join(fetch_population(), by = join_by(geography)) |>
  mutate(
    crashes_per_100k      = total_crashes / population * 1e5,
    fatal_injury_per_100k = injury_or_fatal_crashes / population * 1e5,
    pct_fatal             = if_else(total_crashes > 0,
                                    fatal_crashes / total_crashes * 100,
                                    NA_real_),
    pct_fatal_or_injury   = if_else(total_crashes > 0,
                                    injury_or_fatal_crashes / total_crashes * 100,
                                    NA_real_)
  )

dir.create("output", showWarnings = FALSE)
write_csv(indicators, "output/pre2015_check_by_mode.csv")

# 4. Chart

# The series is deliberately drawn as two segments split at the dataset break
# rather than one continuous line, so the join is not read as a trend.

series_colours <- setNames(c("#2a78d6", "#eb6834"), geo_levels)

theme_dh <- theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
    strip.text       = element_text(face = "bold", size = 9.5),
    axis.title.y     = element_text(size = 9.5, margin = margin(r = 8)),
    axis.text        = element_text(size = 9),
    plot.title       = element_text(face = "bold", size = 11.5,
                                    margin = margin(b = 4)),
    plot.caption     = element_text(colour = "grey35", size = 8, hjust = 0)
  )

metric_row <- function(col, row_title, y_lab) {
  indicators |>
    transmute(geography, year, mode, dataset, value = .data[[col]]) |>
    ggplot(aes(year, value, colour = geography)) +
    geom_vline(xintercept = break_year, linetype = "dashed",
               colour = "grey55", linewidth = 0.4) +
    geom_line(aes(group = interaction(geography, dataset)), linewidth = 0.9) +
    geom_point(size = 1.7) +
    facet_wrap(vars(mode), scales = "free_y", nrow = 1) +
    scale_x_continuous(breaks = seq(2010, 2025, 3)) +
    scale_colour_manual(values = series_colours, breaks = geo_levels) +
    expand_limits(y = 0) +
    labs(title = row_title, x = NULL, y = y_lab, colour = NULL) +
    theme_dh
}

check_plot <- (
  metric_row("crashes_per_100k", "All crashes",
             "Crashes per 100,000 residents") /
  metric_row("fatal_injury_per_100k", "Fatal or injury crashes",
             "Crashes per 100,000 residents")
) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title    = "Crashes per 100,000 residents by road user, 2010-2025",
    subtitle = "Not a continuous series. Dashed line marks the 2015 switch from the ConnDOT dataset to MMUCC.",
    caption  = paste(
      "Source: CT Crash Data Repository (ctcrash.uconn.edu). Population: 2020 U.S. Decennial Census, held constant across all years.",
      "The crash form and reporting thresholds changed in 2015, so the step across the dashed line may be due to the change.",
      "2011 is missing property-damage-only reporting from March onwards.",
      sep = "\n"),
    theme = theme_minimal(base_size = 11) +
      theme(plot.title    = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(size = 11, margin = margin(b = 6)),
            plot.caption  = element_text(colour = "grey35", size = 8, hjust = 0))
  ) &
  theme(legend.position = "bottom")

ggsave("output/pre2015_check_rates.png", check_plot,
       width = 11, height = 7, dpi = 300, device = ragg::agg_png)
print(check_plot)

# 5. Printed table

walk(geo_levels, function(geo) {
  message("\n--- ", geo, ": crashes by year (all modes) ---")
  indicators |>
    filter(geography == geo) |>
    group_by(dataset, year) |>
    summarise(crashes         = sum(total_crashes),
              fatal           = sum(fatal_crashes),
              fatal_or_injury = sum(injury_or_fatal_crashes),
              .groups = "drop") |>
    arrange(year) |>
    print(n = Inf)
})

message("\nWrote:\n  ", conndot_file, "  (compiled 2010-2014 ConnDOT counts)",
        "\n  output/pre2015_check_by_mode.csv  (tidy: geography x year x mode, 2010-2025)",
        "\n  output/pre2015_check_rates.png    (2010-2025 sanity-check chart)")
