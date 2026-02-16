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

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

# Explore the data
head(climateSupport)
str(climateSupport)
summary(climateSupport)

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
p_value <- 1 - pchisq(test_stat, df_diff)

cat("Global Null Hypothesis Test (Likelihood Ratio Test):\n")
cat("H0: All slopes = 0 (no effect of predictors)\n")
cat("Test Statistic (Chi-squared):", test_stat, "\n")
cat("Degrees of Freedom:", df_diff, "\n")
cat("P-value:", p_value, "\n")

# Generate stargazer output for model1
stargazer(model1, 
          title = "Logistic Regression: Support for International Environmental Policy",
          label = "tab:model1",
          type = "latex",
          out = "model1_output.txt",
          style = "default",
          notes.align = "c",
          notes = "\\parbox[t]{7cm}{$^{***}p<0.01$; $^{**}p<0.05$; $^*p<0.1$}",
          table.placement = "H")


#####################
# Problem 2
#####################

# Note: We focus on LINEAR contrasts only for interpretation

# Extract the linear coefficient for sanctions
sanctions_linear_coef <- coef(model1)["sanctions.L"]
sanctions_linear_se <- summary(model1)$coefficients["sanctions.L", "Std. Error"]

# Problem 2a: Policy with 160 countries, sanctions increase from 5% to 15%
# Increasing from 5% (level 1) to 15% (level 2) is a change of 1 unit

# Odds ratio for 1 unit increase 
odds_ratio_2a <- exp(sanctions_linear_coef)

cat("\n2a. Policy with 160 countries: Sanctions increase from 5% to 15%\n")
cat("Sanctions Linear Coefficient:", sanctions_linear_coef, "\n")
cat("Odds Ratio (for 1 unit increase in linear contrast):", odds_ratio_2a, "\n")
cat("Percentage change in odds:", (odds_ratio_2a - 1) * 100, "%\n")

# Problem 2b: Policy with 20 countries, sanctions increase from 5% to 15%
# Note: The coefficient is the SAME regardless of number of countries
# because this is an ADDITIVE model (no interaction) and odds ratio is non-conditional metric (i.e., doesn't depend on other predictors in the model)
# So the effect of sanctions on odds is the same

cat("\n2b. Policy with 20 countries: Sanctions increase from 5% to 15%\n")
cat("Sanctions Linear Coefficient:", sanctions_linear_coef, "\n")
cat("Odds Ratio (for 1 unit increase in linear contrast):", odds_ratio_2a, "\n")
cat("Percentage change in odds:", (odds_ratio_2a - 1) * 100, "%\n")
cat("NOTE: In an ADDITIVE model, the effect of sanctions on odds is the same regardless of the number of participating countries\n")


# Problem 2c: Predicting probability that an individual will support policy if there are 80 of 192 countries (lvl 1) participating with no sanctions (lvl 0)
