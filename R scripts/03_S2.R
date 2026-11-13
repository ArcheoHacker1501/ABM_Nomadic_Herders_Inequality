# =============================================================================
# 03_S2.R - Scenario 2 (debt with free exit): H2, compared with S1
# For H2 (Table 1): persistence after 25 years, against S1 (02_S1.R). The
# networks of S2 are compared with those of S3 (H3, 04_S3.R).
# Tables, in figures/S2: run values of S2 (means and 95% CIs). The figures and
# tables that compare S1, S2 and S3 are made by 04_S3.R; the sensitivity
# analysis is in 08_sensitivity.R.

# THIS SCRIPT ONLY PRODUCES TABLES. THE FIGURES FOR 02_S1.R, 03_S2.R AND
# 04_S3.R ARE PLOTTED BY 04_S3.R, WHICH READS THE DATA ITSELF: THE ORDER IN WHICH
# THE SCRIPTS ARE RUN DOES NOT MATTER.
# =============================================================================
source("00_setup.R")
set_fig_dir("S2")   # figures and tables of this script go to figures/S2

f2 <- exchange_finals(read_bs("S2"))

# ---- Run values: means and 95% CIs at each frequency of individual disasters
tab2 <- ci_table(f2, c(m_base, m_net, m_patron), "rate")
print(tab2, n = Inf, width = Inf)
save_table(tab2, "S2_run_values")
