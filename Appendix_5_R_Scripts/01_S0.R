# =============================================================================
# 01_S0.R - Scenario 0 (no exchange): H0
# For H0 (Appendix 1, Table 1): households at the end of the run; Gini of the
# first and of the last 500 years (run values, means and 95% CIs). In S0 the
# run values use every year (no warm-up).
# Figures, in figures/S0: 1 households and pasture used over time; 2 Gini over
# time, with the mean peak of the runs; 3 persistence of the richest 10%. The
# year in which half of the households had disappeared is only in the text
# (run values). The sensitivity analysis of S0 is in 08_sensitivity.R.
# =============================================================================
source("00_setup.R")
set_fig_dir("S0")   # figures and tables of this script go to figures/S0

s0  <- read_bs("S0")
f0  <- finals(s0) |> mutate(rate = rate_lab(individual_disaster_rate))
# Yearly values every 25 years; pasture used: total livestock as a share of the
# carrying capacity, in %
ts0 <- ts_complete(s0, every = 25) |>
  mutate(rate = rate_lab(individual_disaster_rate), pasture_used = 100 * livestock / carrying_capacity)

# ---- Run values: means and 95% CIs at each frequency of individual disasters
m0 <- c("households", "livestock", "half_gone_year", "gini_peak", "gini_peak_year", "gini_first_500",
        "gini_last_500", "mean_gini_wealth", "persistence_25_wealth", "persistence_200_wealth")
tab0 <- f0 |> mutate(half_gone_year = na_if(half_gone_year, -1)) |> ci_table(m0, "rate")
print(tab0, n = Inf, width = Inf)
save_table(tab0, "S0_run_values")

# ---- Survivors at the end of the run (for the text: who is left)
survivors <- f0 |> filter(households > 0) |>
  group_by(rate) |>
  summarise(runs_with_survivors = n(), mean_survivors = mean(households),
            mean_herd_per_survivor = mean(livestock / households), .groups = "drop")
print(survivors)
save_table(survivors, "S0_survivors")

rate_strip <- function(x) paste0(x, " individual disasters")
wide_panels <- theme(panel.spacing.x = unit(5, "mm"), strip.text.x = element_text(size = rel(0.9)))

# ---- Figure 1: households remaining and pasture used over time
rows <- c(households = "Number of households", pasture_used = "Pasture used (%)")
hp <- ts0 |>
  pivot_longer(c(households, pasture_used), names_to = "measure", values_to = "value") |>
  group_by(measure, rate, step) |> summarise(mean_ci(value), .groups = "drop") |>
  mutate(measure = factor(rows[measure], levels = rows))
half <- tibble(measure = factor(rows[["households"]], levels = rows), y = 50)

# Pasture used, for the text: highest point of the mean curve, and mean over
# the runs of the last 500 years (years 1501-2000, as for the Gini)
pasture <- inner_join(
  hp |> filter(measure == rows[["pasture_used"]]) |> group_by(rate) |>
    slice_max(mean, n = 1, with_ties = FALSE) |> ungroup() |>
    select(rate, peak_year = step, peak = mean, peak_lo = lo, peak_hi = hi),
  ts0 |> filter(step > max_years - 500) |> group_by(rate, run) |>
    summarise(v = mean(pasture_used), .groups = "drop") |>
    group_by(rate) |> summarise(mean_ci(v), .groups = "drop") |>
    select(rate, last_500 = mean, last_500_lo = lo, last_500_hi = hi),
  by = "rate")
print(pasture, width = Inf)
save_table(pasture, "S0_pasture_used")

p1 <- ggplot(hp, aes(step, mean)) +
  geom_hline(data = half, aes(yintercept = y), linetype = "dashed", colour = ref_col, linewidth = 0.4) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = pal_scen[["S0"]], alpha = 0.3) +
  geom_line(colour = pal_scen[["S0"]], linewidth = lwd_mean) +
  hover_pts(aes(text = tip(paste0(measure, ", year ", step), mean, lo, hi, n)), colour = pal_scen[["S0"]]) +
  facet_grid(measure ~ rate, switch = "y", labeller = labeller(rate = rate_strip)) +
  scale_x_continuous(breaks = seq(0, 2000, 500)) +
  coord_cartesian(ylim = c(0, 100)) + wide_panels +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90, hjust = 0.5)) +
  labs(title = "S0: households remaining and pasture used",
       subtitle = "Mean of 100 runs with 95% CI; dashed line: half of the households",
       x = "Year", y = NULL,
       caption = paste("Pasture used: total livestock as a share of the carrying capacity.",
                       "A run in which all households are gone counts as 0."))
save_fig(p1, "S0_fig1_households_pasture", height = 105)

# ---- Figure 2: Gini of total wealth over time, with the mean peak of the runs
# The curve is drawn where at least min_runs runs still have households (1:
# wherever the Gini is defined)
min_runs <- 1
gini_ts <- ts0 |> group_by(rate, step) |> summarise(mean_ci(gini_wealth), .groups = "drop") |>
  filter(n >= min_runs)
peak <- inner_join(filter(tab0, measure == "gini_peak"),
                   filter(tab0, measure == "gini_peak_year") |> select(rate, year = mean, year_lo = lo, year_hi = hi),
                   by = "rate") |>
  mutate(label = paste0(fmt3(mean), " (", round(year), ")"),
         what = paste0("Mean peak of the runs, year ", round(year), " [", round(year_lo), ", ", round(year_hi), "]"))
p2 <- ggplot(gini_ts, aes(step, mean)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = pal_scen[["S0"]], alpha = 0.3) +
  geom_line(colour = pal_scen[["S0"]], linewidth = lwd_mean) +
  hover_pts(aes(text = tip(paste0("Year ", step), mean, lo, hi, n)), colour = pal_scen[["S0"]]) +
  pts(data = peak, aes(year, mean, text = tip(what, mean, lo, hi, n)),
      shape = 24, fill = pal_scen[["S0"]], colour = "#0b0b0b", size = 2.4) +
  # label centered 530 years to the right of the triangle: beside it both in the
  # PNG and in the HTML version (plotly ignores hjust)
  geom_text(data = peak, aes(year + 530, mean, label = label), size = 2.6, colour = "#0b0b0b") +
  facet_wrap(~ rate, nrow = 1, labeller = as_labeller(rate_strip)) +
  scale_x_continuous(breaks = seq(0, 2000, 500)) +
  coord_cartesian(ylim = c(0, 1)) + wide_panels +
  labs(title = "S0: Gini of total wealth over time",
       subtitle = "Gini of total wealth among the remaining households, mean with 95% CI",
       x = "Year", y = "Gini",
       caption = paste0("Triangle: mean of the highest Gini of each run, value (year).\n",
                        "It lies above the curve because the runs peak in different years.\n",
                        if (min_runs > 1) paste0("The curve stops when fewer than ", min_runs,
                                                 " runs still have households; ")
                        else "The fewer the runs that still have households, the wider the band; ",
                        "with one household left the Gini is 0."))
save_fig(p2, "S0_fig2_gini_time", height = 85)

# ---- Figure 3: persistence of the richest 10%
# (symbols of the two measures: shape_pers in 00_setup.R)
pers <- tab0 |> filter(measure %in% names(pers_lab)) |>
  mutate(measure = factor(pers_lab[measure], levels = pers_lab))
p3 <- ggplot(pers, aes(rate, mean, shape = measure)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, colour = pal_scen[["S0"]]) +
  pts(aes(text = tip(rate, mean, lo, hi, n)), fill = pal_scen[["S0"]], colour = "white", size = 2.8) +
  facet_wrap(~ measure) + scale_shape_manual(values = shape_pers) + theme(legend.position = "none") +
  scale_y_continuous(labels = percent) + coord_cartesian(ylim = c(0, 1)) +
  labs(title = "S0: persistence of the richest 10%",
       subtitle = "Mean with 95% CI",
       x = "Frequency of individual disasters", y = "Share of the richest 10%")
save_fig(p3, "S0_fig3_persistence", height = 80)
