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

# Kolmogorov–Smirnov test function 
myKS <- function(data) {
  
  # sort data
  x <- sort(data)
  n <- length(x)
  
  # empirical CDF
  ECDF <- ecdf(x)
  empiricalCDF <- ECDF(x)
  
  # theoretical CDF under N(0,1)
  theoreticalCDF <- pnorm(x)
  
  # KS statistic
  D <- max(abs(empiricalCDF - theoreticalCDF))
  
  # Kolmogorov p-value approximation
  k <- 1:100 # we approximate the infinite sum using the first 100 terms.
  terms <- 2 * (-1)^(k-1) * exp(-2 * (k^2) * (D^2) * n) 
    # (-1)^(k-1) creates the alternating signs
    # exp(-2 * (k^2) * (D^2) * n) are the exponential terms that shrink rapidly
  p_value <- sum(terms)
  
  return(list(
    statistic = D,
    p_value = p_value
  ))
  }

# generate data from Cauchy distribution
set.seed(123)
data <- rcauchy(1000, location = 0, scale = 1)

# run KS test
result <- myKS(data)
cat("KS Statistic:", result$statistic, "\np-value:", result$p_value)

# compare with build-in function result
ks.test(data, "pnorm")

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
