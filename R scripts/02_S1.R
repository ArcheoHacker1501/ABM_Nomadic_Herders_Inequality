# =============================================================================
# 02_S1.R - Scenario 1 (gift): H1, compared with S0
# For H1 (Table 1): households at the end of the run, against S0 (01_S0.R);
# Gini of the first and of the last 500 years after the warm-up. S1 has no
# patron-client networks: the gift creates no obligation, and the link only
# decides whom a household asks first.
# Tables, in figures/S1: run values of S1 (means and 95% CIs). The figures and
# tables that compare S1, S2 and S3 are made by 04_S3.R; the sensitivity
# analysis is in 08_sensitivity.R.

# THIS SCRIPT ONLY PRODUCES TABLES. THE FIGURES FOR 02_S1.R, 03_S2.R AND
# 04_S3.R ARE PLOTTED BY 04_S3.R, WHICH READS THE DATA ITSELF: THE ORDER IN WHICH
# THE SCRIPTS ARE RUN DOES NOT MATTER.
# =============================================================================
source("00_setup.R")
set_fig_dir("S1")   # figures and tables of this script go to figures/S1

f1 <- exchange_finals(read_bs("S1"))

# ---- Run values: means and 95% CIs at each frequency of individual disasters
# (no network measures; in S1 the givers' herd and wealth coincide, because
# gifts transfer the animals)
tab1 <- ci_table(f1, c(m_base, "gini_first_500", m_patron), "rate")
print(tab1, n = Inf, width = Inf)
save_table(tab1, "S1_run_values")
