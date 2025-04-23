getwd() # Print the current working directory

###
#1# Summary statistics and graphical summaries of the data
###
# Read the data from the txt file into R
charges <- scan("charges.txt")
#Numerical summaries
summary(charges) # Summary statistics
sd(charges) # Standard deviation
IQR(charges) # Interquartile range
get_mode <- function(x) { # Function to calculate the mode
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]}
mode_charges <- get_mode(charges) # Calculate and print the mode
cat("Mode:", mode_charges, "\n")
# Graphical summaries
# Histogram
hist(charges, 
     main = "Histogram of Medical Costs in 2024", 
     xlab = "Medical Costs (USD)", 
     col = "skyblue")
# Boxplot
boxplot(charges, 
        main = "Boxplot of Medical Costs in 2024", 
        ylab = "Medical Costs (USD)", 
        col = "lightgreen", 
        horizontal = TRUE)
# Density plot
plot(density(charges, from = 0), # Density plot of the charges starting from 0
     main = "Density Plot of Medical Costs in 2024", 
     xlab = "Medical Costs (USD)", 
     col = "purple")

###
#2#
###
#i)
#MLE
lambda_hat <- 1 / mean(charges) 
lambda_hat # Print the estimated λ using MLE
# QQ Plot for Exponential distribution
qqplot(qexp(ppoints(length(charges)), rate = lambda_hat), sort(charges), # sort the charges for the qqplot
       main = "QQ Plot for Exponential Distribution of the charges",
       xlab = "Theoritical Quantiles (Exponential)",
       ylab = "Sample Quantiles",
       pch=19, col="darkgreen")
abline(0,1, col="red", lwd=2) # qqline, reference line
#ii) Log-transform the data and QQ Plot for Normal distribution
log_charges <- log(charges)
# QQ Plot for Normal distribution with mean = 9 and variance = 1
qqnorm(log_charges, 
       main = "QQ Plot for Log-Transformed Data with Normal Distribution N(9,1)", 
       xlab = "Theoritical Quantiles",
       ylab = "Sample Quantiles",
       pch = 19, col = "darkgreen")
qqline(log_charges, col = "red", lwd = 2) # qqline, reference line
# Histogram
hist(log_charges, 
     breaks = 8,
     main = "Histogram of Log-Transformed Charges", 
     xlab = "Log transfromed charges in  USD", 
     ylab = "Frequency",
     col = "skyblue", 
     border = "white")
# Boxplot
boxplot(log_charges,
        main = "Boxplot of Log-Transformed Charges",
        col = "lightgreen",
        xlab = "Log transfromed charges in  USD",
        horizontal = TRUE)

###
#3# Chi squared tests
###
#i)
# Given parameters
lambda_hat <- 7.456e-5
n <- length(charges)
# Generate cumulative probabilities for 9 equal-probability intervals
probs <- seq(0, 1, length.out = 10)
# Calculate the quantile breakpoints for an exponential distribution
charges.breaks <- qexp(probs, rate = lambda_hat)
# Ensure strictly increasing breaks (by removing duplicates if necessary)
charges.breaks <- unique(charges.breaks)
# Replace the last value with Inf to capture the final interval properly
charges.breaks[length(charges.breaks)] <- Inf
# Check if we still have 10 breakpoints (i.e., 9 intervals)
if (length(charges.breaks) != 10) {
  stop("Breakpoints are not unique enough to create 9 intervals. Try adjusting lambda_hat or check the data distribution.")
}
# Now bin the data
charges.cut <- cut(charges, breaks = charges.breaks, right = FALSE, include.lowest = TRUE)
obs.f.exp <- as.numeric(table(charges.cut))  # Observed frequencies
# Expected frequency per bin under H0
exp.f.exp <- rep(n / 9, 9)
# Chi-squared test statistic
x2.exp <- sum((obs.f.exp - exp.f.exp)^2 / exp.f.exp)
# Degrees of freedom: 9 intervals - 1 constraint - 1 estimated parameter
df <- 9 - 1 - 1
# p-value
pval.exp <- 1 - pchisq(x2.exp, df)
# Output results
list(
  "Observed Frequencies" = obs.f.exp,
  "Expected Frequencies" = exp.f.exp,
  "Chi-squared Statistic" = x2.exp,
  "Degrees of Freedom" = df,
  "p-value" = pval.exp
)
#i)
# Log-transform the charges
log_charges <- log(charges)
# Parameters under H0
mu <- 9
sigma <- 1
n <- length(log_charges)
# Equal-probability quantiles for 9 bins
probs <- seq(0, 1, length.out = 10)
log_breaks <- qnorm(probs, mean = mu, sd = sigma)
# Ensure the last interval goes to infinity
log_breaks[1] <- -Inf
log_breaks[10] <- Inf
# Bin the log-transformed data
log_charges_cut <- cut(log_charges, breaks = log_breaks, right = FALSE, include.lowest = TRUE)
obs_freq <- as.numeric(table(log_charges_cut))  # Observed frequencies
# Expected frequencies: equal under H0
exp_freq <- rep(n / 9, 9)
# Chi-squared statistic
x2_stat <- sum((obs_freq - exp_freq)^2 / exp_freq)
# Degrees of freedom: 9 intervals - 1 constraint (no parameters estimated here)
df <- 8
# p-value
p_val <- 1 - pchisq(x2_stat, df)
# Output the results
list(
  "Observed Frequencies" = obs_freq,
  "Expected Frequencies" = exp_freq,
  "Chi-squared Statistic" = x2_stat,
  "Degrees of Freedom" = df,
  "p-value" = p_val
)

###
#4# Kolmogorov-Smirnov tests
###
#i)
sort(charges) # Sort the charges
ks.test(charges, pexp, rate = lambda_hat) # Perform the KS test
#ii)
sort(log_charges) # Sort the log-transformed charges
ks.test(log_charges, pnorm, mean = 9, sd = 1) # Perform the KS test 

###
#5# 
###
B <- 10000 # set the number of bootstrap samples
median.bst <- numeric(B) # Initialize the vector to store the medians of the bootstrap samples
for(i in 1:B){ # Loop over the number of bootstrap samples
  boot.sample <- sample(charges, size = 149, replace = TRUE) # Generate a bootstrap sample with replacement and the same size as the original sample 
  median.bst[i] = median(boot.sample)} # Store the median of each bootstrap sample
summary(median.bst) # Summary statistics of the bootstrap medians
bootstrap_CI = quantile(median.bst, c(0.025, 0.975)) # 95% bootstrap CI, 2.5% and 97.5% quantiles
bootstrap_CI
# Histogram of the bootstrap medians
hist(median.bst,
     main = "Histogram of bootstrap medians with 95% CI",
     xlab = "Boostrap medians cost in USD",
     col  = "darkgreen")
abline(v = bootstrap_CI, lwd = 3, col = 'red') # Add vertical lines for the 95% CI
# QQ plot
qqnorm(median.bst, 
       main = "Normal Q-Q Plot of Bootstrap Medians")
qqline(median.bst, col = "blue", lwd = 2)

###
#6#
###
# Sample median (on log scale) and sample size
m_hat <- 9.130089
n <- 149
# Density of N(9,1) at m_hat
f_mhat <- dnorm(m_hat, mean = 9, sd = 1) # gives 0.3955808
f_mhat
# Standard error based on asymptotic theory
se_mhat <- 1 / (2 * f_mhat * sqrt(n)) # gives 0.103548
se_mhat
# 95% CI on log scale
z <- qnorm(0.975) # 1.959964
lower_log <- m_hat - z * se_mhat
upper_log <- m_hat + z * se_mhat
c(lower_log, upper_log) # (8.927, 9.333)
# 95% CI on original scale
lower_original <- exp(lower_log)
upper_original <- exp(upper_log)
c(lower_original, upper_original) # (7533.678, 11305.440)

###
#7#
###
B <- 10000 # Number of bootstraps
median.hyp <- 8500 # Hypothesized median 8,500 USD
lambda.hyp <- log(2) / median.hyp # Hypothesized population median under H0
boot.median.H0 = numeric(B) # Initialize the vector to store the medians of the bootstrap samples
# Generate bootstrap medians under the null hypothesis
for (i in 1:B){
  y.boot = rexp(length(charges), rate = lambda.hyp)
  boot.median.H0[i] = median(y.boot)} # Store the median of each bootstrap sample
# Histogram of the bootstrap medians and the sample median
hist(boot.median.H0, 
     col="darkgreen", 
     main="Histogram of bootstrap medians and sample median")
m.hat = median(charges) # Sample median
abline(v = m.hat, col="red", lwd=2.0) # Add a vertical line for the sample median
boot.pval <- (1 + sum(boot.median.H0 <= m.hat)) / (B + 1)
boot.pval 