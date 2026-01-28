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
lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# 1. Implement the KS Statistic function, D = max_i { i/n - F(x_(i)), F(x_(i)) - (i-1)/n }
calculate_ks_statistic <- function(x, mean = 0, sd = 1) {
  n <- length(x)
  x_sorted <- sort(x)
  
  # Calculate F_(i): The theoretical CDF of the reference distribution (Normal)
  F_i <- pnorm(x_sorted, mean = mean, sd = sd)
  
  # Calculate indices i = 1 to n
  i <- 1:n
  
  # Term 1: i/n - F_(i)
  term1 <- (i / n) - F_i
  
  # Term 2: F_(i) - (i-1)/n
  term2 <- F_i - ((i - 1) / n)
  
  # D is the maximum of all these values
  D <- max(pmax(term1, term2))
  
  return(D)
}

# 2. Implement the CDF
calculate_ks_cdf <- function(d) {

  # We sum enough terms (k) to ensure precision.
  k <- 1:1000 # Why? To approximate infinity. If you used 1:n for the sum, your p-value calculation would change depending on how much data you collected
              
  
  # The exponent part: -(2k-1)^2 * pi^2 / (8d^2)
  numerator <- -((2 * k - 1)^2 * pi^2)
  denominator <- 8 * d^2
  
  # Calculate the sum term
  sum_terms <- exp(numerator / denominator)
  series_sum <- sum(sum_terms)
  
  # Multiply by the leading constant: sqrt(2pi) / d
  prob <- (sqrt(2 * pi) / d) * series_sum
  
  return(prob)
}

# 3. Meta function to perform the full test
custom_ks_test <- function(x) {
  n <- length(x)
  
  # Calculate raw D statistic
  D_stat <- calculate_ks_statistic(x)
  
  # Scale the statistic for the distribution formula
  # The distribution of the KS statistic converges to the Kolmogorov distribution when scaled by sqrt(n).
  d_scaled <- D_stat * sqrt(n)
  
  # Calculate CDF probability at d_scaled
  cdf_prob <- calculate_ks_cdf(d_scaled)
  
  # Calculate P-value, as such, P(D > d) = 1 - P(D <= d)
  p_value <- 1 - cdf_prob
  
  return(list(
    D_statistic = D_stat,
    scaled_d = d_scaled,
    p_value = p_value
  ))
}

# --- Execute function ---

# Generate 1,000 Cauchy random variables
set.seed(123)
data <- rcauchy(1000, location = 0, scale = 1)

# Perform the test comparing Cauchy against a Standard Normal Distribution
result <- custom_ks_test(data)

# Output the results
cat("D Statistic (Raw):", result$D_statistic, "\n")
cat("Scaled Statistic (d):", result$scaled_d, "\n")
cat("P-value:", result$p_value, "\n")


#####################
# Problem 2
#####################

set.seed(123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75 * data$x + rnorm(200, 0, 1.5)

# Visualise generated data
plot(data$x, data$y, main="Scatter plot of y vs x", xlab="x", ylab="y") 

# Define the sum of squared residuals (SSR) function
ssr <- function(par, x, y) {
  beta0 <- par[1]
  beta1 <- par[2]
  y_hat <- beta0 + beta1 * x
  sum((y - y_hat)^2)
}

# Use optim() with BFGS to minimise SSR
init_vals <- c(0, 0)   # initial guesses for beta0 and beta1

fit_bfgs <- optim(
  par = init_vals,
  fn = ssr,
  x = data$x,
  y = data$y,
  method = "BFGS"
)

fit_bfgs$par   # estimated coefficients

# Compare with lm()
fit_lm <- lm(y ~ x, data = data)
coef(fit_lm)
