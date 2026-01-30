##################
#### Stats II ####
##################

###############################
#### Tutorial 2: GLMs ####
###############################

# In today's tutorial, we'll begin to explore GLMs
#     1. Import/wrangle data
#     2. Execute lm() and glm() of RQ
#     3. Compare models

#### Case study
# We're interested in central bank governors, specifically their occupational turnover, for almost all countries in the world starting from the year 1970

#### Creat the dataset
# For this task, we first need data.
# 1. Go to https://kof.ethz.ch/en/data/data-on-central-bank-governors.html and download the data on Central Bank Governors
# https://ethz.ch/content/dam/ethz/special-interest/dual/kof-dam/documents/central_bank_governors/cbg_turnover_v23upload.xlsx
# 2. Gather necessary variables
#    codewdi: Country code or name
#    year
#    time to regular turnover	
#    regular turnover dummy	
#    irregular turnover dummy	
#    legal duration

# MAKE SURE THERE AREN'T MISSING VALUES!

# Now, you've got your dataset

#### Import the data
# Your csv file should now be in the desktop folder. Before opening it, we're going to
# load in uour libraries

# Load necessary libraries
lapply(c("readxl", "tidyverse", "ggplot2", "tidyr"), require, character.only = TRUE)

# set working directory
setwd("/Users/jakoblutkemeier/Library/Mobile Documents/com~apple~CloudDocs/Postgraduate/Hillary Term/Applied Statistics 2/StatsII_2026/tutorials/tutorial_02_my_answers")
## loading the data
library(readxl)
data_raw <- read_excel("cbg_turnover_v23upload.xlsx", sheet = "data v2023")
str(data_raw)
names(data_raw)

#### Wrangling the data
# We should now have a dataset where our variables are at least of the correct type
# However, we need to do a bit of tidying to get the data into a more user-friendly
# format.
data <- data_raw %>%
  select(codewdi, year, `time to regular turnover`, `regular turnover dummy`,
         `irregular turnover dummy`, `legal duration`) %>%
  rename(
    country = codewdi,
    time_to_regular_turnover = `time to regular turnover`,
    regular_turnover = `regular turnover dummy`,
    irregular_turnover = `irregular turnover dummy`,
    legal_duration = `legal duration`
  ) %>%
  mutate(
    country = as.factor(country),
    year = as.integer(year),
    time_to_regular_turnover = as.numeric(time_to_regular_turnover),
    regular_turnover = as.integer(regular_turnover),
    irregular_turnover = as.integer(irregular_turnover),
    legal_duration = as.numeric(legal_duration)
  ) %>%
  drop_na()

str(data)
unique(data$year)
unique(data$time_to_regular_turnover)
unique(data$legal_duration)
unique(data$regular_turnover)

bad_codes <- c(-999, -666, -555, -881)
data <- data %>%
  filter(
    !time_to_regular_turnover %in% bad_codes,
    !legal_duration %in% bad_codes,
    !regular_turnover %in% bad_codes,
    !irregular_turnover %in% bad_codes
  )

unique(data$year)
unique(data$time_to_regular_turnover)
unique(data$legal_duration)
unique(data$regular_turnover)


#### Descriptive patterns in turnover
# Compute the average turnover rate (mean of turnover) by country over the full sample period
country_turnover <- data %>%
  group_by(country) %>%
  summarize(
    avg_irregular_turnover = mean(irregular_turnover, na.rm = TRUE)) %>%
  arrange(desc(avg_irregular_turnover))
view(country_turnover)

# (a) Which five countries have the highest average turnover rates?
top_5_countries <- head(country_turnover, 5)
top_5_countries
  
# (b) Which five have the lowest average turnover rates?
bottom_5_countries <- tail(country_turnover, 5)
bottom_5_countries
  
# (c) Plot the distribution of country‑level average turnover rates (e.g. histogram or density) 
#     Briefly comment on whether high turnover is concentrated in a small set of countries
ggplot(country_turnover, aes(x = avg_irregular_turnover)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Country-Level Average Irregular Turnover Rates",
       x = "Average Irregular Turnover Rate",
       y = "Count of Countries") +
  theme_minimal()


####  Estimate a linear probability model (LPM) with OLS:
  
# (a) Fit lm() with:
  # Outcome: irregular turnover dummy
  # Covariates: 
  #   time to regular turnover	
  #   legal duration
lpm_model <- lm(irregular_turnover ~ time_to_regular_turnover + legal_duration, data = data)
# (b) For a “typical” observation  (e.g. median time to regular turnover & legal duration), compute the predicted probability
median_time_to_regular_turnover <- median(data$time_to_regular_turnover, na.rm = TRUE)
predicted_prob_typical <- predict(lpm_model, newdata = data.frame(
  time_to_regular_turnover = median_time_to_regular_turnover,
  legal_duration = median_legal_duration
))
predicted_prob_typical
# (c) Identify at least one observation for which lm() prediction is below 0 or above 1 and explain why such predictions are problematic for a probability
median_legal_duration <- median(data$legal_duration, na.rm = TRUE)
predicted_prob_typical <- predict(lpm_model, newdata = data.frame(
  time_to_regular_turnover = median_time_to_regular_turnover,
  legal_duration = median_legal_duration
))
predicted_prob_typical
# Using the full sample, construct a plot of predicted probability of turnover vs time to regular turnover:
time_seq <- seq(min(data$time_to_regular_turnover, na.rm = TRUE),
                max(data$time_to_regular_turnover, na.rm = TRUE), length.out = 100)
predicted_probs <- predict(lpm_model, newdata = data.frame(
  time_to_regular_turnover = time_seq,
  legal_duration = median_legal_duration
))
plot(time_seq, predicted_probs, type = "l", col = "blue",
     xlab = "Time to Regular Turnover",
     ylab = "Predicted Probability of Irregular Turnover",
     main = "Predicted Probability of Irregular Turnover vs Time to Regular Turnover")


#### Baseline logistic regression
  

# Estimate a logistic regression with governor turnover as the binary outcome and same covariates using glm(family = "binomial")
  
# (a) Report coefficient estimates and standard errors

# (b) Interpret the sign of each coefficient in terms of how they affect the probability of turnover

# (c) For the same “typical” observation used above, compute the predicted probability of turnover (type = "response"), and compare it to the lm() prediction

#### Compare lm() and glm()  

# (a) Use the lm() to compute fitted values across the observed range of time to regular turnoner, holding legal duration at median value

# (b) Use the logit model to compute fitted probabilities for the same legal duration values

# (c) Plot both curves on the same graph (e.g. blue for lm(), red for glm()) 
  
#### Country heterogeneity and fixed effects

# (a) Introduce country fixed effects into the logit specification using dummy variables 

# (b) Compare the estimated coefficients with and without country fixed effects. How does controlling for unobserved country characteristics affect the relationships w/ turnover?
  
# (c) What kinds of country‑specific factors might be absorbed by these fixed effects in this context
