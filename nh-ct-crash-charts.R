# New Haven vs. Connecticut crash trends by mode, 2015-2025

# Two sets of line charts:
#   1. Rates: crashes per 100,000 residents (all crashes; fatal-or-injury crashes)
#   2. Percent: % of that mode's crashes that were fatal; % that were fatal or injury

# Reads the compiled by-type file written by crash-analysis.R
# (crash_counts_by_town_bytype.csv, committed to git)

# Mode is assigned one-per-crash from the people involved: 
# a crash with any cyclist is "Bicyclists", any other non-motorist makes it "Pedestrians", otherwise "Cars"
#  "Cars" includes single-vehicle crashes

# Crash Severity in the CT export is three-level: K = fatal, A = injury, O = no injury
# So "fatal or injury" = severity A or K = every crash in which someone was hurt or killed

library(tidyverse)
library(tidycensus)
library(patchwork)   # stacks the two metric rows into one figure

bytype_crash_file <- "crash_counts_by_town_bytype.csv"
if (!file.exists(bytype_crash_file))
  stop("'", bytype_crash_file, "' not found. Run crash-analysis.R first to ",
       "compile it from raw_data/.", call. = FALSE)

bytype_crashes <- read_csv(
  bytype_crash_file, show_col_types = FALSE,
  col_types = cols(town = "c", year = "i", crash_type = "c",
                   total_crashes = "i", fatal_crashes = "i",
                   injury_or_fatal_crashes = "i")
)

# Geographies and modes

nh_label    <- "New Haven"
state_label <- "Connecticut (statewide)"
geo_levels  <- c(nh_label, state_label)

# Collision type -> the three road-user modes asked for
mode_levels <- c("Cars", "Pedestrians", "Bicyclists")
mode_from_type <- c("Vehicle/vehicle"    = "Cars",
                    "Pedestrian/vehicle" = "Pedestrians",
                    "Cyclist/vehicle"    = "Bicyclists")

counts <- bind_rows(
  bytype_crashes |> filter(town == nh_label) |> mutate(geography = nh_label),
  bytype_crashes |> group_by(year, crash_type) |>
    summarise(across(ends_with("_crashes"), sum), .groups = "drop") |>
    mutate(geography = state_label)
) |>
  mutate(mode = factor(unname(mode_from_type[crash_type]), levels = mode_levels)) |>
  select(geography, year, mode, total_crashes, fatal_crashes,
         injury_or_fatal_crashes) |>
  # A geography x year x mode cell with no crashes is simply absent -> fill 0
  # so the lines do not gap
  complete(geography, year = 2015:2025, mode,
           fill = list(total_crashes = 0L, fatal_crashes = 0L,
                       injury_or_fatal_crashes = 0L)) |>
  mutate(geography = factor(geography, levels = geo_levels))

# Population denominators: 2020 Decennial Census, held constant across all years

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

population <- fetch_population()

# Tidy indicator table behind both chart sets

indicators <- counts |>
  left_join(population, by = join_by(geography)) |>
  mutate(
    crashes_per_100k          = total_crashes / population * 1e5,
    fatal_injury_per_100k     = injury_or_fatal_crashes / population * 1e5,
    pct_fatal                 = if_else(total_crashes > 0,
                                        fatal_crashes / total_crashes * 100,
                                        NA_real_),
    pct_fatal_or_injury       = if_else(total_crashes > 0,
                                        injury_or_fatal_crashes / total_crashes * 100,
                                        NA_real_)
  )

dir.create("output", showWarnings = FALSE)
write_csv(indicators, "output/nh_ct_by_mode.csv")

# Shared chart styling

series_colours <- setNames(c("#2a78d6", "#eb6834"), geo_levels)

theme_dh <- theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
    strip.text       = element_text(face = "bold", size = 9.5),
    # keep the rotated axis title clear of the tick labels
    axis.title.y     = element_text(size = 9.5, margin = margin(r = 8)),
    axis.text        = element_text(size = 9),
    plot.title       = element_text(face = "bold"),
    plot.caption     = element_text(colour = "grey35", size = 8, hjust = 0)
  )

metric_row <- function(df, row_title, y_lab, pct = FALSE, from_zero = TRUE) {
  p <- ggplot(df, aes(year, value, colour = geography)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.7) +
    facet_wrap(vars(mode), scales = "free_y", nrow = 1) +
    scale_x_continuous(breaks = seq(2015, 2025, 2)) +
    scale_colour_manual(values = series_colours, breaks = geo_levels) +
    labs(title = row_title, x = NULL, y = y_lab, colour = NULL) +
    theme_dh +
    theme(plot.title = element_text(face = "bold", size = 11.5,
                                    margin = margin(b = 4)))

  if (from_zero) p <- p + expand_limits(y = 0)
  if (pct) p <- p + scale_y_continuous(labels = scales::label_percent(scale = 1))
  p
}

# Stack two metric rows into one figure with a single shared legend
stack_rows <- function(top, bottom, title, subtitle, caption) {
  (top / bottom) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = title, subtitle = subtitle, caption = caption,
      theme = theme_minimal(base_size = 11) +
        theme(plot.title    = element_text(face = "bold", size = 14),
              plot.subtitle = element_text(size = 11, margin = margin(b = 6)),
              plot.caption  = element_text(colour = "grey35", size = 8,
                                           hjust = 0))
    ) &
    theme(legend.position = "bottom")
}

# 1. Rates per 100,000 residents

long_value <- function(col) {
  indicators |> transmute(geography, year, mode, value = .data[[col]])
}

rate_plot <- stack_rows(
  metric_row(long_value("crashes_per_100k"), "All crashes",
             "Crashes per 100,000 residents"),
  metric_row(long_value("fatal_injury_per_100k"), "Fatal or injury crashes",
             "Crashes per 100,000 residents"),
  title    = "Crashes per 100,000 residents by road user, 2015-2025",
  subtitle = "New Haven vs. Connecticut statewide, by road user.",
  caption  = paste(
    "Source: CT Crash Data Repository (ctcrash.uconn.edu). Population: 2020 U.S. Decennial Census, held constant across all years.",
    sep = "\n")
)

ggsave("output/nh_ct_rates_per_100k.png", rate_plot,
       width = 11, height = 7, dpi = 300, device = ragg::agg_png)
print(rate_plot)

# 2. Percent fatal, and percent fatal-or-injury

pct_plot <- stack_rows(
  metric_row(long_value("pct_fatal") |> filter(!is.na(value)),
             "Percent fatal", "% of that group's crashes",
             pct = TRUE),
  metric_row(long_value("pct_fatal_or_injury") |> filter(!is.na(value)),
             "Percent fatal or injury", "% of that group's crashes",
             pct = TRUE, from_zero = FALSE),
  title    = "Share of crashes that were fatal, and that caused a fatality or injury, 2015-2025",
  subtitle = "New Haven vs. Connecticut statewide, by road user.",
  caption  = "Source: CT Crash Data Repository (ctcrash.uconn.edu). Crash Severity K = fatal, A = injury, O = no injury; 'fatal or injury' = K or A."
)

ggsave("output/nh_ct_percent_fatal.png", pct_plot,
       width = 11, height = 7, dpi = 300, device = ragg::agg_png)
print(pct_plot)

# Printed tables (year down the rows, one block per geography x mode)

walk(geo_levels, function(geo) {
  message("\n--- ", geo, ": crashes by mode and year ---")
  indicators |>
    filter(geography == geo) |>
    transmute(year, mode,
              crashes = total_crashes,
              fatal = fatal_crashes,
              fatal_or_injury = injury_or_fatal_crashes,
              per_100k = round(crashes_per_100k, 1),
              pct_fatal = round(pct_fatal, 2),
              pct_fatal_inj = round(pct_fatal_or_injury, 1)) |>
    arrange(mode, year) |>
    print(n = Inf, width = Inf)
})

message("\nWrote:\n  output/nh_ct_by_mode.csv          (tidy: geography x year x mode)",
        "\n  output/nh_ct_rates_per_100k.png   (crash rates per 100k)",
        "\n  output/nh_ct_percent_fatal.png    (% fatal and % fatal-or-injury)")
