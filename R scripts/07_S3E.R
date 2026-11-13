# =============================================================================
# 07_S3E.R - Scenario 3+E (debt with costly exit, external resources): H4,
# and the figures of the +E experiment
# For H4 (Table 1): as in S1+E, against S3 at 10% individual disasters; in
# each combination, persistence in S3+E against both S3 and S1+E (external
# resources add to debt only if it is higher than both).
# Tables, in figures/S3+E: run values of S3+E by channel and level of the
# inflow (means and 95% CIs).
# Figures, one panel per channel, every line starting from the same scenario
# without inflow: 1 losses in disasters of the recipients and of the other
# households (S3+E); 2 persistence of the richest 10% after 25 and for 200
# years (S1+E and S3+E); 3 Gini of total wealth (S3+E). S0+E and S1+E have no
# figures of their own (05_S0E.R, 06_S1E.R).
# =============================================================================
source("00_setup.R")
set_fig_dir("S3+E")   # figures and tables of this script go to figures/S3+E

# Run values of the +E scenarios (pasture used and share of the clients in the
# largest network from the years recorded, every 100) and of the same
# scenarios without inflow at 10% individual disasters
runs_E <- function(key) {
  d <- read_bs(key)
  exchange_finals(d, ts_complete(d, every = 100)) |> prep_E()
}
ref_10 <- function(key) read_bs(key) |> finals() |> filter(individual_disaster_rate == 0.1)
fe3 <- runs_E("S3E"); f_ref3 <- ref_10("S3")
fe1 <- runs_E("S1E"); f_ref1 <- ref_10("S1")

# ---- Run values of S3+E by channel and level
tabE <- ci_table(fe3, c(m_base, m_net, m_E), c("channel", "inflow_level"))
print(tabE, n = Inf, width = Inf)
save_table(tabE, "S3E_run_values")

# ---- Figures: colors of the series (scenario colors; other households in
# gray) and symbols (circle; in figure 2 those of shape_pers)
col_E <- c(Recipients = pal_scen[["S3"]], `Other households` = "#8a8984",
           S1E = pal_scen[["S1"]], S3E = pal_scen[["S3"]])
point <- c(point = 21)

# ---- Figure 1: losses in disasters of the recipients and of the other
# households (S3+E). With the diffuse channel every household is a
# recipient, so there are no other households
loss <- bind_rows(grid_E(fe3, f_ref3, "recipient_loss_share", "Recipients", ref_measure = "disaster_loss_share"),
                  grid_E(fe3, f_ref3, "other_loss_share", "Other households", ref_measure = "disaster_loss_share") |>
                    filter(channel_lab != lab_chan[["diffuse"]])) |>
  mutate(shape = "point", series = factor(series, levels = c("Recipients", "Other households")))
p1 <- plot_E(loss, col_E, point, "S3+E: losses in disasters",
             paste0("Share lost: animals lost in disasters, net of replacement with the goods, over the herds struck.\n",
                    "No inflow: S3 at 10% individual disasters. With the diffuse channel every household is a recipient."),
             "Share lost") +
  scale_y_continuous(labels = percent) + coord_cartesian(ylim = c(0, 0.6))
save_fig(p1, "S3E_fig1_losses", height = 75)

# ---- Figure 2: persistence of the richest 10% in S1+E and S3+E: after 25
# years (top row) and for 200 years without interruption (bottom row)
pers_row <- c(persistence_25_wealth = "After 25 years", persistence_200_wealth = "For 200 years")
pe <- bind_rows(lapply(names(pers_lab), function(m) {
  bind_rows(grid_E(fe1, f_ref1, m, "S1E"), grid_E(fe3, f_ref3, m, "S3E"))
})) |>
  mutate(shape = pers_lab[measure],
         row = factor(pers_row[measure], levels = pers_row))
p2 <- plot_E(pe, col_E, shape_pers, "S1+E and S3+E: persistence of the richest 10%",
             paste0("Top row: still in the richest 10% after 25 years; bottom row: in the richest 10% for 200 years ",
                    "without interruption.\nNo inflow: S1 and S3 at 10% individual disasters."),
             "Share of the richest 10%", rows = vars(row)) +
  scale_y_continuous(labels = percent) + coord_cartesian(ylim = c(0, 1))
save_fig(p2, "S3E_fig2_persistence", height = 140)

# ---- Figure 3: Gini of total wealth in S3+E, mean over the years after the
# warm-up
gi <- grid_E(fe3, f_ref3, "mean_gini_wealth", "S3E") |> mutate(shape = "point")
p3 <- plot_E(gi, col_E, point, "S3+E: inequality of total wealth",
             paste0("Gini of total wealth (the animals owned, lent ones included), ",
                    "mean over the years\nafter the warm-up. No inflow: S3 at 10% individual disasters."),
             "Gini") +
  coord_cartesian(ylim = c(0, 1)) + theme(legend.position = "none")
save_fig(p3, "S3E_fig3_gini", height = 75)
