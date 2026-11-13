# =============================================================================
# 00_setup.R - common setup for the analysis of the Xiongnu ABM experiments
# Paper: Capital Conversion, Debt and Interconnectivity (NetLogo 7.0.4,
# BehaviorSpace table output). Every other script starts with
# source("00_setup.R"), so keep all the scripts in the same folder and set the
# working directory to it (RStudio: Session > Set Working Directory >
# To Source File Location).
# =============================================================================

# ---- 1. Paths: change data_dir to the folder that holds the -table.csv files
data_dir <- "C:/Users/franc/OneDrive/Desktop/CORSI_ISAW/Seminar_ Eurasian Interconnectivity/Final_Paper/NetLogo/Results"
# PNG and HTML files and CSV tables go to figures, in the folder of each
# script: after source("00_setup.R") every script calls set_fig_dir(), for
# example set_fig_dir("S0") for figures/S0
fig_root <- file.path(data_dir, "figures")
out_dir  <- fig_root
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
set_fig_dir <- function(folder, root = fig_root) {
  out_dir <<- file.path(root, folder)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  message("Figures and tables of this script: ", out_dir)
}

# ---- 2. Packages
pkgs <- c("readr", "dplyr", "tidyr", "ggplot2", "plotly", "htmlwidgets", "scales", "stringr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, function(p) suppressPackageStartupMessages(library(p, character.only = TRUE))))

# ---- 3. Files (names as saved by BehaviorSpace)
files <- c(
  S0      = "Scenario_0_Main_experiment_H0-H3-table.csv",
  S1      = "Scenario_1_Main_experiment_H0-H3-table.csv",
  S2      = "Scenario_2_Main_experiment_H0-H3-table.csv",
  S3      = "Scenario_3_Main_experiment_H0-H3-table.csv",
  S0E     = "Scenario_0_E_Interconnectivity_experiment_H4-table.csv",
  S1E     = "Scenario_1_E_Interconnectivity_experiment_H4-table.csv",
  S3E     = "Scenario_3_E_Interconnectivity_experiment_H4-table.csv",
  S0_sens = "Scenario_0_Sensitivity_analysis_Appendix_1-table.csv",
  S1_sens = "Scenario_1_Sensitivity_analysis_Appendix_1-table.csv",
  S2_sens = "Scenario_2_Sensitivity_analysis_Appendix_1-table.csv",
  S3_sens = "Scenario_3_Sensitivity_analysis_Appendix_1-table.csv"
)

# Parameters (constant within a run); every other column is a measure
param_cols <- c("number_of_households", "initial_herd", "carrying_capacity", "growth_rate",
                "individual_disaster_rate", "general_disaster_rate", "general_disaster_hit", "loss_rate",
                "dissolution_threshold", "poverty_threshold", "patron_threshold", "help_threshold",
                "fixed_transfer", "patron_choice", "share_of_offspring", "max_years", "warm_up",
                "fixed_seed", "inflow_level", "channel")

# ---- 4. Reading
# BehaviorSpace table files have 6 lines of metadata before the header.
# Column names become snake_case: "[run number]" -> run, "gini-wealth" -> gini_wealth.
read_bs <- function(key) {
  d <- readr::read_csv(file.path(data_dir, files[[key]]), skip = 6, na = c("NA", ""),
                       show_col_types = FALSE, progress = FALSE)
  names(d) <- names(d) |> str_remove_all("\\[|\\]") |> str_replace_all("[- ]", "_")
  rename(d, run = run_number)
}

# Last recorded row of each run: the run values (means over the years after
# the warm-up) are those of this row
finals <- function(d) {
  d |> group_by(run) |> slice_max(step, n = 1, with_ties = FALSE) |> ungroup()
}

# Yearly values on a regular grid of years. A run that stopped because all
# households dissolved counts 0 households and 0 animals in the later years;
# the other yearly values (Gini, networks) are not defined there and stay NA.
# The Gini of a year with no households is also set to NA.
ts_complete <- function(d, every) {
  params <- d |> group_by(run) |> slice_min(step, n = 1, with_ties = FALSE) |> ungroup() |>
    select(run, any_of(param_cols))
  grid <- tidyr::expand_grid(run = unique(d$run), step = seq(0, max(d$step), by = every))
  obs <- d |> filter(step %% every == 0) |> select(-any_of(param_cols))
  grid |>
    left_join(obs, by = c("run", "step")) |>
    left_join(params, by = "run") |>
    mutate(households = coalesce(households, 0), livestock = coalesce(livestock, 0)) |>
    mutate(across(any_of(c("gini_wealth", "gini_herds")), ~ if_else(households == 0, NA_real_, .x)))
}

# ---- 5. Statistics
# Mean over the runs in which the measure is defined, with its 95% confidence
# interval (t distribution) and the number of runs in which it is not defined
mean_ci <- function(x) {
  n_all <- length(x)
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(tibble(n = 0L, excluded = n_all, mean = NA_real_, lo = NA_real_, hi = NA_real_))
  m <- mean(x)
  h <- if (n > 1) qt(0.975, n - 1) * sd(x) / sqrt(n) else NA_real_
  tibble(n = n, excluded = n_all - n, mean = m, lo = m - h, hi = m + h)
}

# Long table of means and CIs for several measures, by groups
ci_table <- function(d, measures, by) {
  d |>
    select(all_of(c(by, measures))) |>
    pivot_longer(all_of(measures), names_to = "measure", values_to = "value") |>
    group_by(across(all_of(c(by, "measure")))) |>
    summarise(mean_ci(value), .groups = "drop")
}
# The analysis rules of the paper (Analysis) are applied to these tables of
# means and 95% CIs: the scripts compute no verdicts

# ---- Measures of the exchange scenarios (S1-S3) and of the +E experiments, the
# same in every script. base: as in all scenarios; network: only where there
# are patron-client networks, that is, in the scenarios with loans (S2, S3 and
# S3+E: in S1 and S1+E the gift creates no obligation); patron: patron wealth
# (social storage), only in the text of S1-S3; E: external resources (share of
# the herds struck that is lost, net of replacement, by the recipients, the
# other households and all households)
m_base   <- c("households", "half_gone_year", "pasture_last_500", "mean_gini_wealth", "mean_gini_herds",
              "gini_last_500", "persistence_25_wealth", "persistence_200_wealth")
m_net    <- c("mean_number_of_networks", "mean_network_size_run", "mean_largest_network", "share_largest",
              "mean_network_duration", "share_of_open_networks")
m_patron <- c("patron_herd_change", "patron_wealth_change", "patron_crisis_survival")
m_E      <- c("recipient_loss_share", "other_loss_share", "disaster_loss_share")

# Run values of an exchange scenario: the last row of each run, plus two values
# computed from the yearly values (every 25 years): the pasture used in the
# last 500 years (total livestock as a share of the carrying capacity, in %),
# and the share of all clients that are in the largest network (years after
# the warm-up with at least one client; NA in S0+E, which has no networks).
# half_gone_year is NA in the runs in which half of the households never
# disappeared. The main experiments record every 25 years; for the +E
# experiments (every 100 years) pass ts = ts_complete(d, every = 100), or the
# missing years would count as empty.
exchange_finals <- function(d, ts = ts_complete(d, every = 25)) {
  nets <- "largest_network" %in% names(ts)
  extra <- ts |> group_by(run) |>
    summarise(pasture_last_500 = mean(100 * livestock[step > max_years - 500] /
                                        carrying_capacity[step > max_years - 500]),
              share_largest = if (nets) {
                ok <- step > warm_up & !is.na(number_of_clients) & number_of_clients > 0
                if (any(ok)) mean(largest_network[ok] / number_of_clients[ok]) else NA_real_
              } else NA_real_,
              .groups = "drop")
  finals(d) |>
    mutate(rate = rate_lab(individual_disaster_rate), half_gone_year = na_if(half_gone_year, -1)) |>
    left_join(extra, by = "run")
}

# ---- Sensitivity analysis (Appendix 3): one variant at a time
base_values <- list(carrying_capacity = 25000, general_disaster_rate = 0.1, fixed_transfer = 0,
                    poverty_threshold = 60, help_threshold = 500, patron_choice = "random",
                    share_of_offspring = 0.8)
sens_variant <- function(d) {
  for (nm in names(base_values)) if (!nm %in% names(d)) d[[nm]] <- base_values[[nm]]
  d |> mutate(variant = case_when(
    carrying_capacity == 10000   ~ "Carrying capacity 10,000",
    carrying_capacity == 50000   ~ "Carrying capacity 50,000",
    general_disaster_rate == 0.2 ~ "General disasters 20% (individual 5%)",
    fixed_transfer == 60         ~ "Fixed transfer 60",
    fixed_transfer == 100        ~ "Fixed transfer 100",
    poverty_threshold == 100     ~ "Poverty threshold 100",
    help_threshold == 800        ~ "Help threshold 800",
    patron_choice == "richest"   ~ "Richest patron",
    share_of_offspring == 0.3    ~ "Share of offspring 30%",
    share_of_offspring == 0.7    ~ "Share of offspring 70%",
    share_of_offspring == 0.9    ~ "Share of offspring 90%",
    TRUE                         ~ "Base values"))
}
# Each variant is compared with the main experiment at the same frequency of
# individual disasters: 5% for the variant with general disasters at 20%, else 10%
variant_rate <- function(variant) ifelse(grepl("General disasters", variant), 0.05, 0.10)
# Runs of a scenario for a variant: the variant itself if the scenario has it,
# otherwise (the parameter plays no role there) its main experiment at the same
# frequency of individual disasters
runs_for <- function(sens, main, v) {
  if (v %in% sens$variant) filter(sens, variant == v) else
    filter(main, individual_disaster_rate == variant_rate(v))
}

# ---- 6. Labels, colors and theme (the same in every figure)
lab_scen <- c(S0 = "S0 (no exchange)", S1 = "S1 (gift)", S2 = "S2 (debt, free exit)",
              S3 = "S3 (debt, costly exit)", S0E = "S0+E (no exchange)", S1E = "S1+E (gift)",
              S3E = "S3+E (debt, costly exit)")
# Scenario colors (validated as a set for color-vision deficiencies; S1-S3 stay
# distinct by color alone, so in the figures over time all lines are solid)
pal_scen   <- c(S0 = "#eda100", S1 = "#2a78d6", S2 = "#e87ba4", S3 = "#008300")
shape_scen <- c(S0 = 21, S1 = 24, S2 = 22, S3 = 23)
# Channels of the external goods (in the +E figures they are columns of
# panels, so they have no colors of their own)
lab_chan   <- c(top = "Top (richest 10%)", frontier = "Frontier (fixed quarter)",
                diffuse = "Diffuse (all households)")
# Persistence of the richest 10%: the two measures keep the same symbols in
# every figure (circle: after 25 years; downward triangle: 200 years)
pers_lab   <- c(persistence_25_wealth  = "Still in the richest 10% after 25 years",
                persistence_200_wealth = "In the richest 10% for 200 years without interruption")
shape_pers <- setNames(c(21, 25), pers_lab)
# Width of the mean lines in the figures over time, the same in every script
lwd_mean <- 0.4
# Frequencies of individual disasters and inflow levels as ordered factors
rate_levels  <- c("5%", "7.5%", "10%", "12.5%")
rate_lab     <- function(x) factor(paste0(x * 100, "%"), levels = rate_levels)
inflow_lab   <- function(x) factor(paste0(x * 100, "%"), levels = c("1.5%", "3%", "6%"))
ref_col      <- "#52514e"   # reference lines and text
fmt3         <- function(x) formatC(x, digits = 3, format = "fg", flag = "#")

theme_paper <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#e8e7e3", linewidth = 0.3),
      axis.line        = element_line(colour = ref_col, linewidth = 0.3),
      axis.ticks       = element_line(colour = ref_col, linewidth = 0.3),
      axis.text        = element_text(colour = ref_col),
      axis.title       = element_text(colour = "#0b0b0b"),
      plot.title       = element_text(colour = "#0b0b0b", face = "bold", size = rel(1.1)),
      plot.subtitle    = element_text(colour = ref_col),
      plot.caption     = element_text(colour = ref_col, size = rel(0.8), hjust = 0),
      plot.title.position = "plot",
      strip.text       = element_text(colour = "#0b0b0b", face = "bold", hjust = 0),
      legend.position  = "bottom",
      legend.title     = element_text(colour = "#0b0b0b"),
      legend.text      = element_text(colour = "#0b0b0b")
    )
}
theme_set(theme_paper())

scale_scen <- function(keys) {
  list(scale_colour_manual(values = pal_scen[keys], labels = lab_scen[keys], name = NULL),
       scale_fill_manual(values = pal_scen[keys], labels = lab_scen[keys], name = NULL),
       scale_shape_manual(values = shape_scen[keys], labels = lab_scen[keys], name = NULL))
}
scale_scen_lines <- function(keys) {
  list(scale_colour_manual(values = pal_scen[keys], labels = lab_scen[keys], name = NULL),
       scale_fill_manual(values = pal_scen[keys], labels = lab_scen[keys], name = NULL))
}

# Point layers that also carry a tooltip text for plotly (ggplot2 warns about
# the unknown "text" aesthetic, which only plotly uses)
pts <- function(mapping, ...) suppressWarnings(geom_point(mapping, ...))
# invisible points on a line: no mark in the PNG, a hover target in plotly
hover_pts <- function(mapping, ...) suppressWarnings(geom_point(mapping, alpha = 0, size = 1.5, ...))
tip <- function(what, mean, lo, hi, n) {
  paste0(what, "<br>Mean: ", fmt3(mean), " [", fmt3(lo), ", ", fmt3(hi), "]<br>Runs: ", n)
}

# ---- 7. Saving and showing: PNG for the paper, HTML (plotly) for exploration.
# Every figure is also drawn in R: the static plot in the Plots pane and the
# interactive one in the Viewer pane (RStudio). Set show_plots <- FALSE to only
# write the files (for example when running the scripts in batch).
show_plots <- TRUE
save_fig <- function(p, name, width = 170, height = 105) {
  ggsave(file.path(out_dir, paste0(name, ".png")), p, width = width, height = height, units = "mm",
         dpi = 300, bg = "white")
  w <- suppressWarnings(plotly::ggplotly(p, tooltip = "text"))
  # Legend of the HTML version: ggplotly names an entry after the data values
  # ("S1", or "S1,S1" when several aesthetics share a variable) and repeats it
  # for every layer. Keep the first value, write the scenario labels and show
  # each entry once; clicking it hides every layer of that series.
  seen <- character(0)
  w$x$data <- lapply(w$x$data, function(tr) {
    if (!is.null(tr$name) && nzchar(tr$name)) {
      nm <- strsplit(sub("^\\((.*)\\)$", "\\1", tr$name), ",", fixed = TRUE)[[1]][1]
      if (nm %in% names(lab_scen)) nm <- lab_scen[[nm]]
      tr$name <- nm
      tr$legendgroup <- nm
      if (!isFALSE(tr$showlegend)) {
        tr$showlegend <- !(nm %in% seen)
        seen <<- c(seen, nm)
      }
    }
    tr
  })
  # Panel labels on more than one line (e.g. "Number of households\nat the end"):
  # more room above the panels, so that they do not cover the title
  n_br <- vapply(w$x$layout$annotations, function(a) {
    if (is.null(a$text)) 0 else sum(gregexpr("\n|<br", a$text[1])[[1]] > 0)
  }, numeric(1))
  if (length(n_br) && max(n_br) > 0 && !is.null(w$x$layout$margin$t)) {
    w$x$layout$margin$t <- w$x$layout$margin$t + 18 * max(n_br)
  }
  selfcontained <- nzchar(Sys.getenv("RSTUDIO_PANDOC")) || nzchar(Sys.which("pandoc"))
  htmlwidgets::saveWidget(w, file.path(out_dir, paste0(name, ".html")),
                          selfcontained = selfcontained, title = name)
  if (selfcontained) unlink(file.path(out_dir, paste0(name, "_files")), recursive = TRUE)
  if (show_plots) {
    print(p)   # static plot (Plots pane)
    print(w)   # interactive plot (Viewer pane)
  }
  invisible(w)
}

# Tables of means and CIs are also written as CSV next to the figures,
# with a byte-order mark so that Excel reads the plus-minus sign correctly
save_table <- function(x, name) {
  readr::write_excel_csv(x, file.path(out_dir, paste0(name, ".csv")))
  invisible(x)
}

# ---- 8. Interconnectivity experiment (H4): S0+E, S1+E and S3+E
# Labels for channel and level of the inflow
prep_E <- function(d) {
  d |> mutate(channel_lab = factor(lab_chan[channel], levels = lab_chan),
              level       = inflow_lab(inflow_level))
}

# Figures of the +E experiment: one panel per channel. On the x axis the
# yearly inflow, starting from the same scenario without inflow (main
# experiment at 10% individual disasters), so that every line starts from
# its reference; mean of 100 runs with 95% CI.
level_E <- c("No inflow", "1.5%", "3%", "6%")

# Means and CIs of one measure in every combination, plus the reference without
# inflow (ref_measure in f_ref) repeated in every panel. series: key of the line
# (a scenario key such as "S3E", or a label without commas)
grid_E <- function(fe, f_ref, measure, series, ref_measure = measure) {
  ref <- tidyr::expand_grid(channel_lab = factor(lab_chan, levels = lab_chan),
                            mean_ci(f_ref[[ref_measure]])) |>
    mutate(level = "No inflow")
  ci_table(fe, measure, c("channel_lab", "level")) |>
    mutate(level = as.character(level)) |>
    select(-measure) |>
    bind_rows(ref) |>
    mutate(level = factor(level, levels = level_E), measure = measure, series = series)
}

# One line per series; colours: named by series; shapes: named by the values
# of the column shape of d; rows: the variables of the rows of panels, if any
dodge_E <- position_dodge(width = 0.4)
plot_E <- function(d, colours, shapes, title, caption, ylab, rows = NULL) {
  lab_series <- function(k) ifelse(k %in% names(lab_scen), lab_scen[k], k)
  ggplot(d, aes(level, mean, colour = series, fill = series, group = series)) +
    geom_line(linewidth = lwd_mean, position = dodge_E) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.4, position = dodge_E) +
    pts(aes(shape = shape, text = tip(paste(lab_series(series), channel_lab, level, sep = ", "),
                                      mean, lo, hi, n)),
        colour = "white", size = 2.3, stroke = 0.4, position = dodge_E) +
    facet_grid(rows = rows, cols = vars(channel_lab)) +
    scale_colour_manual(values = colours, labels = lab_series, name = NULL) +
    scale_fill_manual(values = colours, labels = lab_series, name = NULL) +
    scale_shape_manual(values = shapes, guide = "none") +
    guides(colour = guide_legend(override.aes = list(shape = 21))) +
    labs(title = title, subtitle = "Mean of 100 runs with 95% CI", caption = caption,
         x = "Yearly inflow (share of the carrying capacity)", y = ylab)
}

message("Setup done. Data: ", data_dir, "\nFigures and tables: ", out_dir)
