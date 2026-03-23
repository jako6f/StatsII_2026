# replication_analysis.R
# Replication scaffold for:
# von Soest & Wahman (2014)
# "Not all dictators are equal: Coups, fraudulent elections and the selective targeting of democratic sanctions"

# -----------------------------
# 0) Setup
# -----------------------------

setwd("/Users/jakoblutkemeier/Documents/projects/replication-project-democratic-sanctions")

required_packages <- c(
  "readr", "dplyr", "fixest", "modelsummary", "marginaleffects",
  "ggplot2", "broom", "tibble", "purrr"
)

to_install <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

library(readr)
library(dplyr)
library(fixest)
library(modelsummary)
library(marginaleffects)
library(ggplot2)
library(broom)
library(tibble)
library(purrr)

# Output folders
table_dir <- "reports/tables"
figure_dir <- "reports/figures"

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 1) Load parsed data
# -----------------------------

df <- read_csv("data/vSW_replicationJPR.csv", show_col_types = FALSE)

# -----------------------------
# 2) Reconstruct lagged dependent variables
# -----------------------------

# Most lagged covariates already exist in the parsed Stata data.
# We reconstruct the lagged dependent variables needed for the paper's
# sample restrictions (exclude country-years already under democratic sanction at t-1).

df <- df %>%
  arrange(ccode_qog, year) %>%
  group_by(ccode_qog) %>%
  mutate(
    l_dmhr_sancgoal = lag(dmhr_sancgoal),
    l_dmhrsancgoalBMR = lag(dmhrsancgoalBMR),
    l_eu_dmhrsancgoal = lag(eu_dmhrsancgoal),
    l_us_dmhrsancgoal = lag(us_dmhrsancgoal)
  ) %>%
  ungroup()

# -----------------------------
# 3) Helper functions
# -----------------------------

# Fit clustered logistic regression
fit_logit <- function(fml, data) {
  feglm(
    fml = fml,
    data = data,
    family = binomial(),
    cluster = ~ ccode_qog
  )
}

# Build model sample from lag restriction + required variables + listwise deletion
make_sample <- function(data, dep_var, lag_dep_var, rhs_vars) {
  vars_to_keep <- unique(c(dep_var, rhs_vars, "ccode_qog"))
  data %>%
    filter(.data[[lag_dep_var]] == 0) %>%
    select(all_of(vars_to_keep)) %>%
    na.omit()
}

# Save either modelsummary model tables or pre-built data-frame tables as LaTeX
save_model_table <- function(models, filename, title = NULL) {
  # force LaTeX output file extension
  tex_filename <- sub("\\.[^.]+$", ".tex", filename)
  if (!grepl("\\.tex$", tex_filename, ignore.case = TRUE)) {
    tex_filename <- paste0(tex_filename, ".tex")
  }
  
  out_path <- file.path(table_dir, tex_filename)
  
  if (inherits(models, "data.frame")) {
    datasummary_df(
      models,
      output = out_path,
      title = title,
      fmt = 3
    )
  } else {
    gof_map <- tibble::tribble(
      ~raw,   ~clean, ~fmt,
      "nobs", "N",    0
    )
    
    modelsummary(
      models,
      output = out_path,
      title = title,
      estimate = "{estimate}{stars}",
      statistic = "({std.error})",
      stars = TRUE,
      coef_omit = "Intercept",
      gof_map = gof_map,
      fmt = 3,
      escape = FALSE
    )
  }
  
  invisible(out_path)
}

# Save tidy coefficients as CSV
save_model_csv <- function(models, model_names, filename) {
  out <- purrr::map2_dfr(
    models,
    model_names,
    ~ broom::tidy(.x, conf.int = TRUE) %>%
      mutate(model = .y, .before = 1)
  )
  
  write_csv(out, file.path(table_dir, filename))
}

# Predicted probabilities for binary scenarios using datagrid()
# Unspecified covariates are held at their typical values by datagrid().
scenario_predictions <- function(model, focal_var, fixed_values, scenario_label) {
  grid_args <- c(
    list(model = model),
    fixed_values,
    setNames(list(c(0, 1)), focal_var)
  )
  
  grid <- do.call(datagrid, grid_args)
  
  predictions(model, newdata = grid, conf_level = 0.90) %>%
    as_tibble() %>%
    mutate(
      scenario = scenario_label,
      focal = focal_var,
      .before = 1
    )
}

# Discrete 0 -> 1 change for a binary focal variable
scenario_first_difference <- function(model, focal_var, fixed_values, scenario_label) {
  base_grid <- do.call(
    datagrid,
    c(list(model = model), fixed_values)
  )
  
  comparisons(
    model,
    variables = focal_var,
    newdata = base_grid,
    conf_level = 0.90
  ) %>%
    as_tibble() %>%
    mutate(
      scenario = scenario_label,
      focal = focal_var,
      .before = 1
    )
}

# Prediction curve for a continuous focal variable
curve_predictions <- function(model, focal_var, values, fixed_values, curve_label) {
  grid_args <- c(
    list(model = model),
    fixed_values,
    setNames(list(values), focal_var)
  )
  
  grid <- do.call(datagrid, grid_args)
  
  predictions(model, newdata = grid, conf_level = 0.90) %>%
    as_tibble() %>%
    mutate(
      curve = curve_label,
      focal = focal_var,
      .before = 1
    )
}

# Formatting helpers for Table II
fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f"), x))
}

fmt_ci <- function(low, high, digits = 3) {
  ifelse(
    is.na(low) | is.na(high),
    NA_character_,
    paste0(fmt_num(low, digits), "–", fmt_num(high, digits))
  )
}

build_table2_column <- function(baseline_pred, baseline_var, coup_fd, election_fd) {
  baseline_row <- baseline_pred %>%
    filter(.data[[baseline_var]] == 0)
  
  c(
    fmt_num(baseline_row$estimate[1]),
    fmt_ci(baseline_row$conf.low[1], baseline_row$conf.high[1]),
    fmt_num(coup_fd$estimate[1]),
    fmt_ci(coup_fd$conf.low[1], coup_fd$conf.high[1]),
    fmt_num(election_fd$estimate[1]),
    fmt_ci(election_fd$conf.low[1], election_fd$conf.high[1])
  )
}

# -----------------------------
# 4) Table I: Models 1-4
# -----------------------------

# Model 1: HTW (Western sanctions)
rhs_m1 <- c(
  "couprev", "ifhpol", "contelection", "deltafhcl",
  "lnondmhrsancgoal", "t1nodmhrsancgoal", "t2nodmhrsancgoal", "t3nodmhrsancgoal"
)

df_m1 <- make_sample(df, "dmhr_sancgoal", "l_dmhr_sancgoal", rhs_m1)

m1 <- fit_logit(
  dmhr_sancgoal ~ couprev + ifhpol + contelection + deltafhcl +
    lnondmhrsancgoal + t1nodmhrsancgoal + t2nodmhrsancgoal + t3nodmhrsancgoal,
  data = df_m1
)

# Model 2: BMR (Western sanctions)
rhs_m2 <- c(
  "couprev", "ifhpol", "contelection", "deltafhcl",
  "lnondmhrsancgoal", "nodemsancBMRt1", "nodemsancBMRt2", "nodemsancBMRt3"
)

df_m2 <- make_sample(df, "dmhrsancgoalBMR", "l_dmhrsancgoalBMR", rhs_m2)

m2 <- fit_logit(
  dmhrsancgoalBMR ~ couprev + ifhpol + contelection + deltafhcl +
    lnondmhrsancgoal + nodemsancBMRt1 + nodemsancBMRt2 + nodemsancBMRt3,
  data = df_m2
)

# Model 3: HTW (EU sanctions)
rhs_m3 <- c(
  "couprev", "ifhpol", "contelection", "deltafhcl",
  "lnondmhrsancgoal", "noeudemsanct1", "noeudemsanct2", "noeudemsanct3"
)

df_m3 <- make_sample(df, "eu_dmhrsancgoal", "l_eu_dmhrsancgoal", rhs_m3)

m3 <- fit_logit(
  eu_dmhrsancgoal ~ couprev + ifhpol + contelection + deltafhcl +
    lnondmhrsancgoal + noeudemsanct1 + noeudemsanct2 + noeudemsanct3,
  data = df_m3
)

# Model 4: HTW (US sanctions)
rhs_m4 <- c(
  "couprev", "ifhpol", "contelection", "deltafhcl",
  "lnondmhrsancgoal", "nousdemsanct1", "nousdemsanct2", "nousdemsanct3"
)

df_m4 <- make_sample(df, "us_dmhrsancgoal", "l_us_dmhrsancgoal", rhs_m4)

m4 <- fit_logit(
  us_dmhrsancgoal ~ couprev + ifhpol + contelection + deltafhcl +
    lnondmhrsancgoal + nousdemsanct1 + nousdemsanct2 + nousdemsanct3,
  data = df_m4
)

table_I_models <- list(
  "\\shortstack{(1) HTW\\\\(Western sanctions)}" = m1,
  "\\shortstack{(2) BMR\\\\(Western sanctions)}" = m2,
  "\\shortstack{(3) HTW\\\\(EU sanctions)}"      = m3,
  "\\shortstack{(4) HTW\\\\(US sanctions)}"      = m4
)

save_model_table(
  table_I_models,
  filename = "table_I_models_1_4.html",
  title = "Triggers of democratic sanctions"
)

save_model_csv(
  models = table_I_models,
  model_names = names(table_I_models),
  filename = "table_I_models_1_4_coefficients.csv"
)

# -----------------------------
# 5) Table II: Predicted probabilities
# -----------------------------

# Model 1 (Western sanctions)
m1_baseline_pred <- scenario_predictions(
  model = m1,
  focal_var = "couprev",
  fixed_values = list(
    contelection = 0,
    lnondmhrsancgoal = 0,
    t1nodmhrsancgoal = 10,
    t2nodmhrsancgoal = 100,
    t3nodmhrsancgoal = 1000
  ),
  scenario_label = "Model 1: coup scenarios"
)

m1_coup_fd <- scenario_first_difference(
  model = m1,
  focal_var = "couprev",
  fixed_values = list(
    contelection = 0,
    lnondmhrsancgoal = 0,
    t1nodmhrsancgoal = 10,
    t2nodmhrsancgoal = 100,
    t3nodmhrsancgoal = 1000
  ),
  scenario_label = "Model 1: coup first difference"
)

m1_election_fd <- scenario_first_difference(
  model = m1,
  focal_var = "contelection",
  fixed_values = list(
    couprev = 0,
    lnondmhrsancgoal = 0,
    t1nodmhrsancgoal = 10,
    t2nodmhrsancgoal = 100,
    t3nodmhrsancgoal = 1000
  ),
  scenario_label = "Model 1: election first difference"
)

# Model 3 (EU sanctions)
m3_baseline_pred <- scenario_predictions(
  model = m3,
  focal_var = "couprev",
  fixed_values = list(
    contelection = 0,
    lnondmhrsancgoal = 0,
    noeudemsanct1 = 10,
    noeudemsanct2 = 100,
    noeudemsanct3 = 1000
  ),
  scenario_label = "Model 3: coup scenarios"
)

m3_coup_fd <- scenario_first_difference(
  model = m3,
  focal_var = "couprev",
  fixed_values = list(
    contelection = 0,
    lnondmhrsancgoal = 0,
    noeudemsanct1 = 10,
    noeudemsanct2 = 100,
    noeudemsanct3 = 1000
  ),
  scenario_label = "Model 3: coup first difference"
)

m3_election_fd <- scenario_first_difference(
  model = m3,
  focal_var = "contelection",
  fixed_values = list(
    couprev = 0,
    lnondmhrsancgoal = 0,
    noeudemsanct1 = 10,
    noeudemsanct2 = 100,
    noeudemsanct3 = 1000
  ),
  scenario_label = "Model 3: election first difference"
)

# Model 4 (US sanctions)
m4_baseline_pred <- scenario_predictions(
  model = m4,
  focal_var = "couprev",
  fixed_values = list(
    contelection = 0,
    lnondmhrsancgoal = 0,
    nousdemsanct1 = 10,
    nousdemsanct2 = 100,
    nousdemsanct3 = 1000
  ),
  scenario_label = "Model 4: coup scenarios"
)

m4_coup_fd <- scenario_first_difference(
  model = m4,
  focal_var = "couprev",
  fixed_values = list(
    contelection = 0,
    lnondmhrsancgoal = 0,
    nousdemsanct1 = 10,
    nousdemsanct2 = 100,
    nousdemsanct3 = 1000
  ),
  scenario_label = "Model 4: coup first difference"
)

m4_election_fd <- scenario_first_difference(
  model = m4,
  focal_var = "contelection",
  fixed_values = list(
    couprev = 0,
    lnondmhrsancgoal = 0,
    nousdemsanct1 = 10,
    nousdemsanct2 = 100,
    nousdemsanct3 = 1000
  ),
  scenario_label = "Model 4: election first difference"
)

table_II <- tibble(
  Statistic = c(
    "Baseline probability (no coup or controversial election)",
    "90% CI",
    "Coup",
    "90% CI",
    "Controversial election",
    "90% CI"
  ),
  `Western sanction` = build_table2_column(
    baseline_pred = m1_baseline_pred,
    baseline_var = "couprev",
    coup_fd = m1_coup_fd,
    election_fd = m1_election_fd
  ),
  `EU sanction` = build_table2_column(
    baseline_pred = m3_baseline_pred,
    baseline_var = "couprev",
    coup_fd = m3_coup_fd,
    election_fd = m3_election_fd
  ),
  `US sanction` = build_table2_column(
    baseline_pred = m4_baseline_pred,
    baseline_var = "couprev",
    coup_fd = m4_coup_fd,
    election_fd = m4_election_fd
  )
)

save_model_table(
  table_II,
  filename = "table_II_predicted_probabilities.html",
  title = "Change in predicted probability of democratic sanctions"
)

write_csv(
  table_II,
  file.path(table_dir, "table_II_predicted_probabilities.csv")
)

write_csv(
  bind_rows(
    m1_baseline_pred, m1_coup_fd, m1_election_fd,
    m3_baseline_pred, m3_coup_fd, m3_election_fd,
    m4_baseline_pred, m4_coup_fd, m4_election_fd
  ),
  file.path(table_dir, "table_II_postestimation_components.csv")
)

# -----------------------------
# 6) Table III: Models 5-7
# -----------------------------

# Model 5
rhs_m5 <- c(
  "couprev", "contelection", "protest",
  "lwdigdpgrocap", "lwdiinflationgdpfrac", "lgdpconstantcapita1000",
  "lwestorgtie", "lblackknight", "lwesttradelog", "lwdi_fdi", "loilmil",
  "lnondmhrsancgoal", "lagree2un_westimp2",
  "t1nodmhrsancgoal", "t2nodmhrsancgoal", "t3nodmhrsancgoal"
)

df_m5 <- make_sample(df, "dmhr_sancgoal", "l_dmhr_sancgoal", rhs_m5)

m5 <- fit_logit(
  dmhr_sancgoal ~ couprev + contelection + protest +
    lwdigdpgrocap + lwdiinflationgdpfrac + lgdpconstantcapita1000 +
    lwestorgtie + lblackknight + lwesttradelog + lwdi_fdi + loilmil +
    lnondmhrsancgoal + lagree2un_westimp2 +
    t1nodmhrsancgoal + t2nodmhrsancgoal + t3nodmhrsancgoal,
  data = df_m5
)

# Model 6
rhs_m6 <- c(
  "couprev", "contelection", "protest",
  "lwdigdpgrocap", "lwdiinflationgdpfrac", "lgdpconstantcapita1000",
  "lwestorgtie", "lblackknight", "lwesttradelog", "lwdi_fdi", "loilmil",
  "lnondmhrsancgoal", "lagree2un_westimp2",
  "nodemsancBMRt1", "nodemsancBMRt2", "nodemsancBMRt3"
)

df_m6 <- make_sample(df, "dmhrsancgoalBMR", "l_dmhrsancgoalBMR", rhs_m6)

m6 <- fit_logit(
  dmhrsancgoalBMR ~ couprev + contelection + protest +
    lwdigdpgrocap + lwdiinflationgdpfrac + lgdpconstantcapita1000 +
    lwestorgtie + lblackknight + lwesttradelog + lwdi_fdi + loilmil +
    lnondmhrsancgoal + lagree2un_westimp2 +
    nodemsancBMRt1 + nodemsancBMRt2 + nodemsancBMRt3,
  data = df_m6
)

# Model 7
rhs_m7 <- c(
  "couprev", "contelection", "lwdi_fdi",
  "lgdpconstantcapita1000", "protest", "lwdiinflationgdpfrac",
  "lwdigdpgrocap", "lwestorgtie", "lblackknight", "lwesttradelog",
  "loilmil", "lnondmhrsancgoal", "lagree2un_westimp2",
  "t1nodmhrsancgoal", "t2nodmhrsancgoal", "t3nodmhrsancgoal"
)

df_m7 <- make_sample(df, "dmhr_sancgoal", "l_dmhr_sancgoal", rhs_m7)

m7 <- fit_logit(
  dmhr_sancgoal ~ couprev + contelection * lwdi_fdi +
    lgdpconstantcapita1000 + protest + lwdiinflationgdpfrac +
    lwdigdpgrocap + lwestorgtie + lblackknight + lwesttradelog +
    loilmil + lnondmhrsancgoal + lagree2un_westimp2 +
    t1nodmhrsancgoal + t2nodmhrsancgoal + t3nodmhrsancgoal,
  data = df_m7
)

table_III_models <- list(
  "\\shortstack{(5) HTW\\\\(Western sanctions)}" = m5,
  "\\shortstack{(6) BMR\\\\(Western sanctions)}" = m6,
  "\\shortstack{(7) HTW\\\\(Western sanctions)}" = m7
)

save_model_table(
  table_III_models,
  filename = "table_III_models_5_7.html",
  title = "Determinants of democratic sanctions"
)

save_model_csv(
  models = table_III_models,
  model_names = names(table_III_models),
  filename = "table_III_models_5_7_coefficients.csv"
)



# -----------------------------
# 7) Figure 2: Model 7 margins plot
# -----------------------------

fig2_data <- curve_predictions(
  model = m7,
  focal_var = "lwdi_fdi",
  values = seq(0, 20, by = 2),
  fixed_values = list(
    contelection = c(0, 1),
    couprev = 0
  ),
  curve_label = "Figure 2"
) %>%
  mutate(
    contelection = factor(contelection, levels = c(0, 1), labels = c("No", "Yes"))
  )

write_csv(fig2_data, file.path(table_dir, "figure_2_prediction_grid.csv"))

fig2 <- ggplot(
  fig2_data,
  aes(
    x = lwdi_fdi,
    y = estimate,
    linetype = contelection,
    fill = contelection
  )
) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.20,
    colour = NA
  ) +
  geom_line(linewidth = 0.9) +
  labs(
    x = "Lagged FDI inflows (% of GDP)",
    y = "Predicted probability",
    linetype = "Controversial election",
    fill = "Controversial election"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face = "bold",
      colour = "black",
      hjust = 0,
      margin = margin(b = 12)
    ),
    plot.margin = margin(t = 12, r = 20, b = 12, l = 12),
    axis.title = element_text(colour = "black"),
    axis.text = element_text(colour = "black"),
    legend.title = element_text(colour = "black"),
    legend.text = element_text(colour = "black"),
    legend.position = "right"
  )

ggsave(
  filename = file.path(figure_dir, "figure_2_fdi_by_election_status.png"),
  plot = fig2,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


# -----------------------------
# 8) Replication extension:
# controversial election x high political closeness
# -----------------------------

# This section tests whether internationally controversial elections are less
# likely to trigger democratic sanctions when the target is highly politically
# aligned with the West. "High political closeness" is defined as the top
# quartile of UN voting agreement with the West in the estimation sample.

# -----------------------------
# 8a) Build one common estimation sample
# -----------------------------

# RHS variables (predictors) used to define the extension sample.
rhs_ext_polclose <- c(
  "couprev",
  "contelection",
  "protest",
  "lwdigdpgrocap",
  "lwdiinflationgdpfrac",
  "lgdpconstantcapita1000",
  "lagree2un_westimp2",
  "lnondmhrsancgoal",
  "t1nodmhrsancgoal",
  "t2nodmhrsancgoal",
  "t3nodmhrsancgoal"
)

# Construct the estimation sample used for the extension models.
# This applies the same sample restrictions as in the main replication,
# including the lagged dependent variable condition for sanction onset.
ext_polclose_sample <- make_sample(
  data = df,
  dep_var = "dmhr_sancgoal",
  lag_dep_var = "l_dmhr_sancgoal",
  rhs_vars = rhs_ext_polclose
)

# Define the threshold for "high political closeness" as the
# 75th percentile of UN voting similarity within the estimation sample.
polclose_cutoff <- unname(
  quantile(ext_polclose_sample$lagree2un_westimp2, probs = 0.75, na.rm = TRUE)
)

# Create a dummy variable for regimes in the top quartile of political closeness
ext_polclose_sample <- ext_polclose_sample %>%
  mutate(
    high_polclose = if_else(lagree2un_westimp2 >= polclose_cutoff, 1, 0)
  )

# -----------------------------
# 8b) Estimate baseline and interaction model on the exact same sample
# -----------------------------

m_ext_polclose_base <- fit_logit(
  dmhr_sancgoal ~
    couprev +
    contelection +
    high_polclose +
    protest +
    lwdigdpgrocap +
    lwdiinflationgdpfrac +
    lgdpconstantcapita1000 +
    lnondmhrsancgoal +
    t1nodmhrsancgoal + t2nodmhrsancgoal + t3nodmhrsancgoal,
  data = ext_polclose_sample
)

m_ext_polclose_int <- fit_logit(
  dmhr_sancgoal ~
    couprev +
    contelection * high_polclose +
    protest +
    lwdigdpgrocap +
    lwdiinflationgdpfrac +
    lgdpconstantcapita1000 +
    lnondmhrsancgoal +
    t1nodmhrsancgoal + t2nodmhrsancgoal + t3nodmhrsancgoal,
  data = ext_polclose_sample
)

# -----------------------------
# 8c) Main extension model table
# -----------------------------

ext_models <- list(
  "Baseline model" = m_ext_polclose_base,
  "Interaction model" = m_ext_polclose_int
)

save_model_table(
  models = ext_models,
  filename = "extension_polclose_threshold_models.html",
  title = "Replication extension: political closeness threshold interaction"
)

# Optional but useful for checking exact estimates outside the HTML table
save_model_csv(
  models = ext_models,
  model_names = names(ext_models),
  filename = "extension_polclose_threshold_coefficients.csv"
)

# -----------------------------
# 8d) Model fit comparison
# -----------------------------

fit_comparison <- tibble(
  Model = c("Baseline", "Interaction"),
  N = c(nobs(m_ext_polclose_base), nobs(m_ext_polclose_int)),
  LogLik = c(as.numeric(logLik(m_ext_polclose_base)), as.numeric(logLik(m_ext_polclose_int))),
  AIC = c(AIC(m_ext_polclose_base), AIC(m_ext_polclose_int)),
  BIC = c(BIC(m_ext_polclose_base), BIC(m_ext_polclose_int))
) %>%
  mutate(
    LogLik = round(LogLik, 3),
    AIC = round(AIC, 3),
    BIC = round(BIC, 3)
  )

save_model_table(
  fit_comparison,
  filename = "extension_polclose_threshold_fit_comparison.html",
  title = "Model fit comparison for the Replication extension"
)

write_csv(
  fit_comparison,
  file.path(table_dir, "extension_polclose_threshold_fit_comparison.csv")
)

# -----------------------------
# 8e) Descriptive support table for the Replication threshold specification
# -----------------------------

support_threshold <- ext_polclose_sample %>%
  mutate(
    election_status = if_else(
      contelection == 1,
      "Controversial election",
      "No controversial election"
    ),
    political_closeness = if_else(
      high_polclose == 1,
      "Top quartile",
      "Lower 3 quartiles"
    )
  ) %>%
  group_by(election_status, political_closeness) %>%
  summarise(
    N = n(),
    Sanctions = sum(dmhr_sancgoal),
    `Sanction rate` = round(mean(dmhr_sancgoal), 3),
    .groups = "drop"
  )

save_model_table(
  support_threshold,
  filename = "extension_polclose_threshold_support.html",
  title = "Observed sanction rates by election status and political closeness"
)

write_csv(
  support_threshold,
  file.path(table_dir, "extension_polclose_threshold_support.csv")
)

# -----------------------------
# 8f) Predicted probabilities for the 2 x 2 interaction
# -----------------------------

polclose_threshold_grid <- tibble(
  contelection = c(0, 0, 1, 1),
  high_polclose = c(0, 1, 0, 1),
  couprev = 0,
  protest = mean(ext_polclose_sample$protest, na.rm = TRUE),
  lwdigdpgrocap = mean(ext_polclose_sample$lwdigdpgrocap, na.rm = TRUE),
  lwdiinflationgdpfrac = mean(ext_polclose_sample$lwdiinflationgdpfrac, na.rm = TRUE),
  lgdpconstantcapita1000 = mean(ext_polclose_sample$lgdpconstantcapita1000, na.rm = TRUE),
  lnondmhrsancgoal = mean(ext_polclose_sample$lnondmhrsancgoal, na.rm = TRUE),
  t1nodmhrsancgoal = 0,
  t2nodmhrsancgoal = 0,
  t3nodmhrsancgoal = 0
)

polclose_threshold_preds <- marginaleffects::predictions(
  m_ext_polclose_int,
  newdata = polclose_threshold_grid,
  type = "response"
) %>%
  as_tibble() %>%
  transmute(
    `Election status` = if_else(contelection == 1, "Controversial election", "No controversial election"),
    `Political closeness` = if_else(high_polclose == 1, "Top quartile", "Lower 3 quartiles"),
    `Predicted probability` = round(estimate, 3),
    `95% CI` = paste0(
      round(conf.low, 3),
      "–",
      round(conf.high, 3)
    )
  )

save_model_table(
  polclose_threshold_preds,
  filename = "extension_polclose_threshold_predictions.html",
  title = "Predicted probabilities from the replication extension model"
)

write_csv(
  polclose_threshold_preds,
  file.path(table_dir, "extension_polclose_threshold_predictions.csv")
)

# -----------------------------
# 9) Robustness checks for the replication extension
# threshold sensitivity + lag sensitivity
# -----------------------------

# These checks ask:
# 1) Does the extension depend heavily on where "high political closeness" is cut?
# 2) Does the extension change materially if political closeness is lagged by two years?

# Helper: fit one threshold-based interaction model and return a compact summary row
fit_polclose_robustness <- function(sample_data, closeness_var, cutoff_prob, specification_label) {
  cutoff_value <- unname(
    quantile(sample_data[[closeness_var]], probs = cutoff_prob, na.rm = TRUE)
  )
  
  dat <- sample_data %>%
    mutate(
      high_polclose_robust = if_else(.data[[closeness_var]] >= cutoff_value, 1, 0)
    )
  
  mod <- fit_logit(
    dmhr_sancgoal ~
      couprev +
      contelection * high_polclose_robust +
      protest +
      lwdigdpgrocap +
      lwdiinflationgdpfrac +
      lgdpconstantcapita1000 +
      lnondmhrsancgoal +
      t1nodmhrsancgoal + t2nodmhrsancgoal + t3nodmhrsancgoal,
    data = dat
  )
  
  int_row <- broom::tidy(mod, conf.int = TRUE) %>%
    mutate(
      term = case_when(
        term %in% c("contelection:high_polclose_robust", "high_polclose_robust:contelection") ~ "contelection:high_polclose_robust",
        TRUE ~ term
      )
    ) %>%
    filter(term == "contelection:high_polclose_robust")
  
  tibble(
    Specification = specification_label,
    `Closeness variable` = closeness_var,
    `Cutoff percentile` = cutoff_prob,
    `Cutoff value` = round(cutoff_value, 3),
    N = nobs(mod),
    `Interaction estimate` = round(int_row$estimate[1], 3),
    `Std. error` = round(int_row$std.error[1], 3),
    `p-value` = round(int_row$p.value[1], 3),
    `95% CI` = paste0(round(int_row$conf.low[1], 3), "–", round(int_row$conf.high[1], 3)),
    AIC = round(AIC(mod), 3),
    BIC = round(BIC(mod), 3)
  )
}

# -----------------------------
# 9a) Threshold sensitivity using the same 1-year lagged closeness measure
# -----------------------------

threshold_robustness <- bind_rows(
  fit_polclose_robustness(
    sample_data = ext_polclose_sample,
    closeness_var = "lagree2un_westimp2",
    cutoff_prob = 0.75,
    specification_label = "Main extension: 1-year lag, top quartile"
  ),
  fit_polclose_robustness(
    sample_data = ext_polclose_sample,
    closeness_var = "lagree2un_westimp2",
    cutoff_prob = 2/3,
    specification_label = "Threshold sensitivity: 1-year lag, top third"
  ),
  fit_polclose_robustness(
    sample_data = ext_polclose_sample,
    closeness_var = "lagree2un_westimp2",
    cutoff_prob = 0.80,
    specification_label = "Threshold sensitivity: 1-year lag, top 20%"
  )
)

# -----------------------------
# 9b) Lag sensitivity:
# reconstruct a 2-year lag for political closeness and re-estimate
# -----------------------------

df_lag2_polclose <- df %>%
  arrange(ccode_qog, year) %>%
  group_by(ccode_qog) %>%
  mutate(
    l2agree2un_westimp2 = lag(agree2un_westimp2, 2)
  ) %>%
  ungroup()

rhs_ext_polclose_lag2 <- c(
  "couprev",
  "contelection",
  "protest",
  "lwdigdpgrocap",
  "lwdiinflationgdpfrac",
  "lgdpconstantcapita1000",
  "l2agree2un_westimp2",
  "lnondmhrsancgoal",
  "t1nodmhrsancgoal",
  "t2nodmhrsancgoal",
  "t3nodmhrsancgoal"
)

ext_polclose_sample_lag2 <- make_sample(
  data = df_lag2_polclose,
  dep_var = "dmhr_sancgoal",
  lag_dep_var = "l_dmhr_sancgoal",
  rhs_vars = rhs_ext_polclose_lag2
)

lag_robustness <- fit_polclose_robustness(
  sample_data = ext_polclose_sample_lag2,
  closeness_var = "l2agree2un_westimp2",
  cutoff_prob = 0.75,
  specification_label = "Lag sensitivity: 2-year lag, top quartile"
)

# -----------------------------
# 9c) Combine and save robustness summary
# -----------------------------

extension_robustness_summary <- bind_rows(
  threshold_robustness,
  lag_robustness
)

save_model_table(
  extension_robustness_summary,
  filename = "robustness_extension_threshold_lag_summary.html",
  title = "Robustness checks for the replication extension"
)

write_csv(
  extension_robustness_summary,
  file.path(table_dir, "robustness_extension_threshold_lag_summary.csv")
)