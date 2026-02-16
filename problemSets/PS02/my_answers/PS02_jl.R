#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

# Explore the data
head(climateSupport)
str(climateSupport)
summary(climateSupport)

# Convert countries and sanctions to unordered factors with specified baselines
# Baseline for countries: "20 of 192"
# Baseline for sanctions: "None"

climateSupport$countries <- factor(climateSupport$countries, 
                                   levels = c("20 of 192", "80 of 192", "160 of 192"),
                                   ordered = FALSE)

climateSupport$sanctions <- factor(climateSupport$sanctions,
                                   levels = c("None", "5%", "15%", "20%"),
                                   ordered = FALSE)

# Verify the factor levels and baselines
levels(climateSupport$countries)
levels(climateSupport$sanctions)

# Fit an additive logistic regression model

model1 <- glm(choice ~ countries + sanctions, 
              family = binomial(link = "logit"), 
              data = climateSupport)

# Summary of the model
summary(model1)

# Extract and display the global null hypothesis test
# The null hypothesis: H0: all slopes = 0 (no effect of predictors)
# Test statistic: Null deviance - Residual deviance
null_deviance <- model1$null.deviance
residual_deviance <- model1$deviance
df_diff <- model1$df.null - model1$df.residual
test_stat <- null_deviance - residual_deviance
p_value <- pchisq(test_stat, df_diff, lower.tail = FALSE)

cat("Global Null Hypothesis Test (Likelihood Ratio Test):\n")
cat("H0: All slopes = 0 (no effect of predictors)\n")
cat("Test Statistic (Chi-squared):", test_stat, "\n")
cat("Degrees of Freedom:", df_diff, "\n")
cat("P-value:", formatC(p_value, format = "e", digits = 2), "\n")

# Generate stargazer output for model1
stargazer(model1, 
          title = "Logistic Regression: Support for International Environmental Policy",
          label = "tab:model1",
          type = "latex")


#####################
# Problem 2
#####################

# Extract coefficients for interpretation
coef_summary <- summary(model1)$coefficients
coef_summary


# Problem 2a: 160 countries, sanctions from 5% to 15%
sanctions_5pct_coef <- coef_summary["sanctions5%", "Estimate"]
sanctions_15pct_coef <- coef_summary["sanctions15%", "Estimate"]

# Change in log-odds when going from 5% to 15% is:
log_odds_change_2a <- sanctions_15pct_coef - sanctions_5pct_coef
odds_ratio_2a <- exp(log_odds_change_2a)

cat("\n2a. Policy with 160 countries: Sanctions increase from 5% to 15%\n")
cat("Log-odds change:", log_odds_change_2a, "\n")
cat("Odds Ratio:", odds_ratio_2a, "\n")
cat("Percentage change in odds:", (odds_ratio_2a - 1) * 100, "%\n")

# Problem 2b: 20 countries, sanctions from 5% to 15%
# With an additive model (no interaction) and odds ratio being a non-conditional metric (i.e., doesn't depend on other predictors in the model), the effect of sanctions is the same regardless of the number of countries
# So the odds ratio is identical to 2a

cat("\n2b. Policy with 20 countries: Sanctions increase from 5% to 15%\n")
cat("Log-odds change:", log_odds_change_2a, "\n")
cat("Odds Ratio:", odds_ratio_2a, "\n")
cat("Percentage change in odds:", (odds_ratio_2a - 1) * 100, "%\n")
cat("NOTE: In an ADDITIVE model, this is identical to 2a.\n")

# Problem 2c: Predicted probability for 80 countries with no sanctions
# Manual calculation

# eta_hat = intercept + countries80 * 1 + sanctions_none * 0 (since None is reference)
intercept <- coef(model1)["(Intercept)"]
countries_80_coef <- coef(model1)["countries80 of 192"]
eta_hat <- intercept + countries_80_coef
eta_hat
# Apply logistic transformation
predicted_prob_2c <- 1 / (1 + exp(-eta_hat))

cat("\n2c. Predicted probability: 80 countries with no sanctions\n")
cat("Linear predictor (eta):", eta_hat, "\n")
cat("Predicted Probability:", predicted_prob_2c, "\n")
cat("Predicted Probability (percentage):", predicted_prob_2c * 100, "%\n")


#####################
# Problem 3
#####################

# Fit model with interaction term
model_interaction <- glm(choice ~ countries + sanctions + countries:sanctions, 
                         family = binomial(link = "logit"), 
                         data = climateSupport)

# Test for interaction using likelihood ratio test
anova(model1, model_interaction, test = "Chisq")

# Generate stargazer output for comparison
stargazer(model1, model_interaction,
          title = "Model Comparison: Additive vs. Interaction Model",
          label = "tab:model_comparison",
          type = "latex")

