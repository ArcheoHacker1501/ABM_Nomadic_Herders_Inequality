# =============================================================================
# 06_S1E.R - Scenario 1+E (gift, external resources): H4
# For H4 (Appendix 1, Table 1): persistence after 25 years and Gini, against S1
# at 10% individual disasters (02_S1.R); losses in disasters of the recipients
# and of the other households as a check of the mechanism (Analysis). S1+E has
# no patron-client networks, as S1.
# Tables only, in figures/S1+E: run values by channel and level of the inflow
# (means and 95% CIs). The persistence of S1+E is drawn beside that of S3+E in
# figure 2 of 07_S3E.R.
# =============================================================================
source("00_setup.R")
set_fig_dir("S1+E")   # figures and tables of this script go to figures/S1+E

e1    <- read_bs("S1E")
fe    <- exchange_finals(e1, ts_complete(e1, every = 100)) |> prep_E()

# ---- Run values by channel and level: base and
# external-resource measures (pasture used from the years recorded, every 100)
tabE <- ci_table(fe, c(m_base, m_E), c("channel", "inflow_level"))
print(tabE, n = Inf, width = Inf)
save_table(tabE, "S1E_run_values")
