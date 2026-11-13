# =============================================================================
# 08_sensitivity.R - Sensitivity analysis (Appendix 3)
# Each variant is compared with the main experiment at the same frequency of
# individual disasters (variant_rate in 00_setup.R): means and 95% CIs of the
# measures of H0-H3. S0-S3 (the +E experiments have no sensitivity runs). To
# be completed when the paper is done.
# =============================================================================
source("00_setup.R")
# figures and tables go to the folder of each scenario, as in the other scripts:
# figures/S0, figures/S1, figures/S2 and figures/S3

f0 <- read_bs("S0") |> finals() |> mutate(rate = rate_lab(individual_disaster_rate))
f1 <- read_bs("S1") |> finals() |> mutate(rate = rate_lab(individual_disaster_rate))
f2 <- read_bs("S2") |> finals() |> mutate(rate = rate_lab(individual_disaster_rate))
f3 <- read_bs("S3") |> finals() |> mutate(rate = rate_lab(individual_disaster_rate))
sens0 <- read_bs("S0_sens") |> finals() |> sens_variant()
sens1 <- read_bs("S1_sens") |> finals() |> sens_variant()
sens2 <- read_bs("S2_sens") |> finals() |> sens_variant()
sens3 <- read_bs("S3_sens") |> finals() |> sens_variant()

# ---- S0: year in which half of the households had disappeared, by carrying
# capacity (10% individual disasters; 25,000 is the main experiment)
set_fig_dir("S0")
cc0 <- bind_rows(filter(sens0, carrying_capacity != 25000), filter(f0, individual_disaster_rate == 0.10)) |>
  mutate(half_gone_year = na_if(half_gone_year, -1)) |>
  ci_table("half_gone_year", "carrying_capacity")
print(cc0)
save_table(cc0, "S0_sensitivity_half_gone_by_capacity")

# ---- S0: figure (the measures of H0)
sv0_m <- c("households", "gini_first_500", "gini_last_500")
sens_tab <- bind_rows(
  sens0 |> ci_table(sv0_m, "variant"),
  f0 |> filter(individual_disaster_rate %in% c(0.05, 0.10)) |>
    mutate(variant = paste0("Main experiment, individual ", rate)) |>
    ci_table(sv0_m, "variant")
) |> mutate(measure = factor(measure, levels = sv0_m,
                             labels = c("Number of households\nat the end", "Gini, first 500 years",
                                        "Gini, last 500 years")))
p1 <- ggplot(sens_tab, aes(mean, variant)) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, orientation = "y", colour = pal_scen[["S0"]]) +
  pts(aes(text = tip(variant, mean, lo, hi, n)), shape = 21, fill = pal_scen[["S0"]], colour = "white", size = 2.6) +
  facet_wrap(~ measure, scales = "free_x") +
  labs(title = "S0: sensitivity analysis", subtitle = "Mean with 95% CI", x = NULL, y = NULL)
save_fig(p1, "S0_sensitivity", height = 80)

# ---- S1: figure (the measures of H1; S1 has no patron-client networks)
set_fig_dir("S1")
sv1_m <- c("households", "gini_first_500", "gini_last_500")
sv1 <- bind_rows(
  ci_table(sens1, sv1_m, "variant"),
  f1 |> filter(individual_disaster_rate %in% c(0.05, 0.10)) |>
    mutate(variant = paste0("Main experiment, individual ", rate)) |> ci_table(sv1_m, "variant")
) |> mutate(measure = factor(measure, levels = sv1_m,
                             labels = c("Number of households\nat the end", "Gini, first 500 years",
                                        "Gini, last 500 years")))
p2 <- ggplot(sv1, aes(mean, variant)) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, orientation = "y", colour = pal_scen[["S1"]]) +
  pts(aes(text = tip(variant, mean, lo, hi, n)), shape = 24, fill = pal_scen[["S1"]], colour = "white", size = 2.6) +
  facet_wrap(~ measure, scales = "free_x") +
  labs(title = "S1: sensitivity analysis", subtitle = "Mean with 95% CI", x = NULL, y = NULL)
save_fig(p2, "S1_sensitivity", height = 95)

# ---- S2 against S1 (H2) and S3 against S2 (H3): the test scenario and its
# reference in each variant
pair_plot <- function(sens_t, sens_r, main_r, kt, kr, measures, labels, title, subtitle) {
  sv <- bind_rows(lapply(unique(sens_t$variant), function(v) {
    bind_rows(filter(sens_t, variant == v) |> mutate(scen = kt),
              runs_for(sens_r, main_r, v) |> mutate(scen = kr)) |>
      mutate(variant = v) |> ci_table(measures, c("scen", "variant"))
  })) |> mutate(measure = factor(measure, levels = measures, labels = labels))
  ggplot(sv, aes(mean, variant, colour = scen, fill = scen, shape = scen)) +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, orientation = "y", position = position_dodge(width = 0.5)) +
    pts(aes(text = tip(paste(lab_scen[scen], variant, sep = ", "), mean, lo, hi, n)), colour = "white",
        size = 2.4, position = position_dodge(width = 0.5)) +
    facet_wrap(~ measure, scales = "free_x") + scale_scen(c(kr, kt)) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL)
}
p3 <- pair_plot(sens2, sens1, f1, "S2", "S1", "persistence_25_wealth", "Persistence after 25 years",
                "S2 against S1: sensitivity analysis",
                paste0("Mean with 95% CI; S1 runs of the same variant, or of its main experiment\n",
                       "when the parameter plays no role in S1"))
set_fig_dir("S2")
save_fig(p3, "S2_sensitivity", width = 130, height = 110)
p4 <- pair_plot(sens3, sens2, f2, "S3", "S2",
                c("mean_network_size_run", "mean_network_duration", "persistence_25_wealth"),
                c("Clients per network", "Network duration (years)", "Persistence after 25 years"),
                "S3 against S2: sensitivity analysis", "Mean with 95% CI")
set_fig_dir("S3")
save_fig(p4, "S3_sensitivity", width = 190, height = 110)
