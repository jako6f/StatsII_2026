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

lapply(c("nnet", "MASS", "survival", "eha", "stargazer", "sampleSelection"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data on child mortality by mother's background and child gender
data("child")

str(child)

# keep only variables needed and drop missing values
child_q1 <- na.omit(child[, c("enter", "exit", "event", "m.age", "sex")])

# fit Cox proportional hazards model
child_cox <- coxph(Surv(enter, exit, event) ~ m.age + sex, data = child_q1)

# model output
summary(child_cox)

# 95% confidence intervals for hazard ratios
exp(confint(child_cox))

stargazer(
  child_cox,
  type = "latex",
  title = "Cox Proportional Hazards Model of Child Mortality",
  dep.var.labels = "Hazard of child death",
  covariate.labels = c("Mother's age", "Female"),
  digits = 3,
  single.row = FALSE,
  no.space = TRUE
)

#####################
# Problem 2
#####################

# load data
disaster_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/refs/heads/main/datasets/disaster_response.csv")

# keep only variables needed for Q2
disaster_q2 <- disaster_data[, c(
  "binContribution",
  "originalContributionMillionUSDLogged",
  "occurrences",
  "deathsEM",
  "normalizedDamageEMLogged"
)]

# key structure / missingness
summary(disaster_q2)
colSums(is.na(disaster_q2))

# is binContribution really 0/1, and how balanced is it?
table(disaster_q2$binContribution)
# -> data are highly imbalanced, but not problematically so for this task

# does the logged contribution variable use a placeholder (-25.328) for no-donation cases?
tapply(
  disaster_q2$originalContributionMillionUSDLogged,
  disaster_q2$binContribution,
  summary
)
# -> for binContribution = 0, the logged contribution is always -25.328
# -> for binContribution = 1, it varies meaningfully.

# in a Heckman model, the outcome equation is observed only when selection = 1
# so set the logged contribution amount to NA when no contribution occurred
disaster_q2$originalContributionMillionUSDLogged[
  disaster_q2$binContribution == 0
] <- NA

# keep rows with non-missing selection indicator and predictors;
# for the outcome, missing values are allowed when selection = 0
disaster_q2 <- subset(
  disaster_q2,
  !is.na(binContribution) &
    !is.na(occurrences) &
    !is.na(deathsEM) &
    !is.na(normalizedDamageEMLogged) &
    !(binContribution == 1 & is.na(originalContributionMillionUSDLogged))
)

# convert selection variable to a two-level factor
# level "1" is the selected state (donation occurs)
disaster_q2$binContribution <- factor(disaster_q2$binContribution, levels = c(0, 1))

# fit Heckman selection model by maximum likelihood
disaster_heckman <- selection(
  selection = binContribution ~ occurrences + deathsEM + normalizedDamageEMLogged,
  outcome   = originalContributionMillionUSDLogged ~ occurrences + deathsEM + normalizedDamageEMLogged,
  data      = disaster_q2,
  method    = "ml"
)

# full model summary
summary(disaster_heckman)



#####################
# separate LaTeX tables for Q2
#####################

# build two clean coefficient tables from your fitted output
selection_tab <- data.frame(
  Term = c("(Intercept)", "occurrences", "deathsEM", "normalizedDamageEMLogged"),
  Estimate = c(-1.3039428, 0.0192811, 0.0118603, 0.0210385),
  `Std. Error` = c(0.0180255, 0.0016553, 0.0007308, 0.0009035),
  `t value` = c(-72.34, 11.65, 16.23, 23.29),
  `Pr(>|t|)` = c("< 0.001", "< 0.001", "< 0.001", "< 0.001"),
  check.names = FALSE
)

outcome_tab <- data.frame(
  Term = c("(Intercept)", "occurrences", "deathsEM", "normalizedDamageEMLogged"),
  Estimate = c(0.413489, -0.001849, 0.005801, -0.018056),
  `Std. Error` = c(0.407490, 0.006353, 0.002031, 0.005234),
  `t value` = c(1.015, -0.291, 2.856, -3.450),
  `Pr(>|t|)` = c("0.310", "0.771", "0.004", "< 0.001"),
  check.names = FALSE
)

# selection-equation table
stargazer(
  selection_tab,
  summary = FALSE,
  rownames = FALSE,
  type = "latex",
  title = "Heckman Selection Model: Probit Selection Equation",
  label = "tab:q2_selection",
  digits = 4
)

# outcome-equation table
stargazer(
  outcome_tab,
  summary = FALSE,
  rownames = FALSE,
  type = "latex",
  title = "Heckman Selection Model: Outcome Equation",
  label = "tab:q2_outcome",
  digits = 4
)


ancillary_tab <- data.frame(
  Term = c("sigma", "rho"),
  Estimate = c(1.93599, -0.52668),
  `Std. Error` = c(0.10384, 0.09104),
  `t value` = c(18.645, -5.785),
  `Pr(>|t|)` = c("< 0.001", "< 0.001"),
  check.names = FALSE
)

stargazer(
  ancillary_tab,
  summary = FALSE,
  rownames = FALSE,
  type = "latex",
  title = "Heckman Selection Model: Error Terms",
  label = "tab:q2_error_terms",
  digits = 4
)