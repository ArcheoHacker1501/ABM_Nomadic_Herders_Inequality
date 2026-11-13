# =============================================================================
# 05_S0E.R - Scenario 0+E (no exchange, external resources): H4
# Check of the mechanism of H4 (Analysis): losses in disasters of the
# recipients and of the other households. The run values of S0+E use every
# year (no warm-up), as in S0.
# Tables only, in figures/S0+E: run values by channel and level of the inflow
# (means and 95% CIs). S0+E has no figures: in the text the households left at
# the end, against S0 at 10% individual disasters (01_S0.R); the recipients
# cannot dissolve, so what counts is what happens to the others.
# The figures of the +E experiment are made by 07_S3E.R.
# =============================================================================
source("00_setup.R")
set_fig_dir("S0+E")   # figures and tables of this script go to figures/S0+E

e0    <- read_bs("S0E")
fe    <- exchange_finals(e0, ts_complete(e0, every = 100)) |> prep_E()

# ---- Run values by channel and level: base measures (pasture
# used in the last 500 years from the years recorded, every 100) and those of
# the external resources
tabE <- ci_table(fe, c(m_base, m_E), c("channel", "inflow_level"))
print(tabE, n = Inf, width = Inf)
save_table(tabE, "S0E_run_values")
