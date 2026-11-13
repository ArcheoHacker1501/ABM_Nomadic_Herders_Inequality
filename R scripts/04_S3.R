# =============================================================================
# 04_S3.R - Scenario 3 (debt with costly exit): H3, compared with S2, and the
# figures and tables that compare S1, S2 and S3
# For H3 (Table 1), S3 against S2: network size, network duration and
# persistence after 25 years (run values, means and 95% CIs).
# Figures and tables, in figures/S3: run values of S3; S1-S3 figures 1
# households and pasture used, 2 Gini of total wealth and of the herds kept,
# 4 persistence of the richest 10%; S2-S3 figure 3 networks over time and
# network table (S1 has no patron-client networks); S1-S3 patron wealth
# (social storage, only in the text). S0 has its own figures (01_S0.R); the
# sensitivity analysis is in 08_sensitivity.R.
# =============================================================================
source("00_setup.R")
set_fig_dir("S3")   # figures and tables of this script go to figures/S3

keys <- c("S1", "S2", "S3")
raw  <- lapply(setNames(keys, keys), read_bs)
ts_l <- lapply(raw, ts_complete, every = 25)
fs   <- lapply(setNames(keys, keys), function(k) exchange_finals(raw[[k]], ts_l[[k]]) |> mutate(scen = k))
f123 <- bind_rows(fs)
# Yearly values every 25 years. After the last household has gone there are no
# networks; clients per network are defined only in the years with networks.
ts123 <- bind_rows(lapply(keys, function(k) ts_l[[k]] |> mutate(scen = k))) |>
  mutate(rate = rate_lab(individual_disaster_rate),
         pasture_used = 100 * livestock / carrying_capacity,
         number_of_networks = if_else(households == 0, 0, number_of_networks),
         mean_network_size = if_else(number_of_networks > 0, mean_network_size, NA_real_))

# ---- Run values of S3: means and 95% CIs at each frequency of individual disasters
tab3 <- ci_table(fs$S3, c(m_base, m_net, m_patron), "rate")
print(tab3, n = Inf, width = Inf)
save_table(tab3, "S3_run_values")

# ---- S2-S3: network table for the paper: mean plus or minus half of the 95% CI
net_lab <- c(mean_number_of_networks = "Number of networks per year",
             mean_network_size_run   = "Clients per network",
             mean_largest_network    = "Clients of the largest network",
             share_largest           = "Clients in the largest network (%)",
             mean_network_duration   = "Network duration (years)")
net_digits <- c(mean_number_of_networks = 2, mean_network_size_run = 1, mean_largest_network = 1,
                share_largest = 0, mean_network_duration = 1)
net_tab <- ci_table(filter(f123, scen != "S1"), names(net_lab), c("scen", "rate")) |>
  mutate(k = if_else(measure == "share_largest", 100, 1), d = unname(net_digits[measure]),
         cell = if_else(is.na(mean), "NA", sprintf("%.*f \u00b1 %.*f", d, k * mean, d, k * (hi - lo) / 2)),
         Measure = factor(net_lab[measure], levels = net_lab), Scenario = lab_scen[scen]) |>
  select(Measure, Scenario, rate, cell) |>
  pivot_wider(names_from = rate, values_from = cell) |>
  arrange(Measure, Scenario)
print(net_tab, n = Inf, width = Inf)
save_table(net_tab, "S2-S3_network_table")

# ---- S1-S3: patron wealth (social storage), only in the text. Mean yearly
# relative change, in absolute value, of the patrons' direct herd and total
# wealth; how much less total wealth varies; share of the patrons keeping their
# status through a year of general disaster
pw <- ci_table(f123, m_patron, c("scen", "rate")) |>
  select(scen, rate, measure, mean, lo, hi) |>
  pivot_wider(names_from = measure, values_from = c(mean, lo, hi), names_glue = "{measure}_{.value}") |>
  mutate(wealth_varies_less = 1 - patron_wealth_change_mean / patron_herd_change_mean) |>
  select(scen, rate, starts_with("patron_herd"), starts_with("patron_wealth"), wealth_varies_less,
         starts_with("patron_crisis")) |>
  arrange(scen, rate)
print(pw, n = Inf, width = Inf)
save_table(pw, "S1-S3_patron_wealth")

# ---- Figures (S1-S3; figure 3 S2-S3)
rate_strip  <- function(x) paste0(x, " individual disasters")
wide_panels <- theme(panel.spacing.x = unit(5, "mm"), strip.text.x = element_text(size = rel(0.9)),
                     strip.placement = "outside", strip.text.y.left = element_text(angle = 90, hjust = 0.5))
warm_up <- annotate("rect", xmin = 0, xmax = 300, ymin = -Inf, ymax = Inf, fill = "#f0efec", alpha = 0.8)

# Means and CIs of yearly values, one row of panels per measure
yearly <- function(labels, k = keys) {
  ts123 |> filter(scen %in% k) |> select(scen, rate, step, all_of(names(labels))) |>
    pivot_longer(all_of(names(labels)), names_to = "measure", values_to = "value") |>
    group_by(scen, rate, step, measure) |> summarise(mean_ci(value), .groups = "drop") |>
    filter(n > 0) |>
    mutate(measure = factor(labels[measure], levels = labels))
}
lines_s123 <- function(d, title, subtitle, caption, scales = "fixed", k = keys) {
  ggplot(d, aes(step, mean, colour = scen, fill = scen)) +
    warm_up +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2, colour = NA) +
    geom_line(linewidth = lwd_mean) +
    hover_pts(aes(text = tip(paste0(lab_scen[scen], ", ", measure, ", year ", step), mean, lo, hi, n))) +
    facet_grid(measure ~ rate, switch = "y", scales = scales, labeller = labeller(rate = rate_strip)) +
    scale_scen_lines(k) + scale_x_continuous(breaks = seq(0, 2000, 500)) + wide_panels +
    labs(title = title, subtitle = subtitle, caption = caption, x = "Year", y = NULL)
}

# ---- Figure 1: households remaining and pasture used over time
p1 <- lines_s123(yearly(c(households = "Number of households", pasture_used = "Pasture used (%)")),
                 "S1-S3: households remaining and pasture used",
                 "Mean of 100 runs with 95% CI; gray: warm-up",
                 paste("Pasture used: total livestock as a share of the carrying capacity.",
                       "A run in which all households are gone counts as 0.")) +
  coord_cartesian(ylim = c(0, 100))
save_fig(p1, "S1-S3_fig1_households_pasture", height = 110)

# ---- Figure 2: Gini of total wealth (top) and of the herds kept (bottom)
p2 <- lines_s123(yearly(c(gini_wealth = "Gini, total wealth", gini_herds = "Gini, herds kept")),
                 "S1-S3: Gini of total wealth and of the herds kept",
                 "Gini among the remaining households, mean of 100 runs with 95% CI; gray: warm-up",
                 paste0("Total wealth: the animals owned, lent ones included. Herds kept: the animals pastured, ",
                        "borrowed ones included.\nIn S1 the two coincide, because gifts transfer the animals.")) +
  coord_cartesian(ylim = c(0, 1))
save_fig(p2, "S1-S3_fig2_gini_wealth_herds", height = 110)

# ---- Figure 3: networks and clients per network over time, in S2 and S3 (S1
# has no patron-client networks)
p3 <- lines_s123(yearly(c(number_of_networks = "Number of networks", mean_network_size = "Clients per network"),
                        k = c("S2", "S3")),
                 "S2-S3: patron-client networks over time",
                 "Mean of 100 runs with 95% CI; gray: warm-up",
                 paste0("Number of networks: patrons with at least one active client; a run in which all households are gone ",
                        "counts 0 networks.\nClients per network: mean over the networks of each year, in the years ",
                        "with at least one network."),
                 scales = "free_y", k = c("S2", "S3")) + expand_limits(y = 0)
save_fig(p3, "S2-S3_fig3_networks", height = 110)

# ---- Figure 4: persistence of the richest 10% (symbols of the two measures:
# shape_pers in 00_setup.R; scenarios by color and position)
pe <- ci_table(f123, names(pers_lab), c("scen", "rate")) |>
  mutate(measure = factor(pers_lab[measure], levels = pers_lab))
dodge <- position_dodge(width = 0.55)
p4 <- ggplot(pe, aes(rate, mean, colour = scen, fill = scen, group = scen)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, position = dodge) +
  pts(aes(shape = measure, text = tip(paste(lab_scen[scen], rate), mean, lo, hi, n)),
      colour = "white", size = 2.6, stroke = 0.5, position = dodge) +
  facet_wrap(~ measure) +
  scale_colour_manual(values = pal_scen[keys], labels = lab_scen[keys], name = NULL, guide = "none") +
  scale_fill_manual(values = pal_scen[keys], labels = lab_scen[keys], name = NULL) +
  scale_shape_manual(values = shape_pers, guide = "none") +
  guides(fill = guide_legend(override.aes = list(shape = 22, colour = NA, size = 3.5))) +
  scale_y_continuous(labels = percent) + coord_cartesian(ylim = c(0, 1)) +
  labs(title = "S1-S3: persistence of the richest 10%",
       subtitle = "Mean with 95% CI",
       x = "Frequency of individual disasters", y = "Share of the richest 10%")
save_fig(p4, "S1-S3_fig4_persistence", height = 85)
