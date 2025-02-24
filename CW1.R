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
#Graph
plot(charges, 
     main = "Scatter plot of the charges in 2024 per patient in USD", 
     xlab = "patient number", 
     ylab = "charges value in USD",
     col = "blue") 
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

###
#3# Chi squared tests
###
#i)
charges.breaks = seq(0, max(charges), length.out = 10) # Define 9 cells to bin the data into
charges.breaks # Print the 9 cells 
charges.cut = cut(charges, breaks = charges.breaks, right = F) # Bin the data into the 9 cells
charges.table <- table(charges.cut) # Count the data in each bin
prob.exp <- numeric(9) # Initialize the vector to store the probabilities
exp.f.exp <- numeric(9) # Initialize the vector to store the expected frequencies
for (i in 1:(length(charges.breaks) - 1)) {
  prob.exp[i] <- pexp(charges.breaks[i+1], rate = lambda_hat) - pexp(charges.breaks[i], rate = lambda_hat)
  exp.f.exp[i] <- prob.exp[i] * length(charges)}
exp.f.exp # Print expected frequencies
obs.f.exp <- as.numeric(charges.table) # Observed frequencies
obs.f.exp # Print observed frequencies
x2.exp <- sum((obs.f.exp - exp.f.exp)^2 / exp.f.exp) # Calculate the chi-squared statistic
x2.exp # Print the chi-squared statistic
pval.exp <- 1 - pchisq(x2.exp, df = 7) # Calculate the p-value using the chi-squared distribution and the degrees of freedom
pval.exp  # Print the p-value
#ii)
log_charges.breaks = seq(min(log_charges), max(log_charges), length.out = 10)  # Define 9 cells to bin the data into
log_charges.breaks # Print the 9 cells 
log_charges.cut = cut(log_charges, breaks = log_charges.breaks, right = F) # Bin the data into the 9 cells
log_charges.table <- table(log_charges.cut) # Count the data in each bin
prob.norm <- numeric(9) # Initialize the vector to store the probabilities
exp.f.norm <- numeric(9) # Initialize the vector to store the expected frequencies
for (i in 1:(length(log_charges.breaks) - 1)) {
  prob.norm[i] <- pnorm(log_charges.breaks[i+1], mean = 9, sd = 1) - pnorm(log_charges.breaks[i], mean = 9, sd = 1)
  exp.f.norm[i] <- prob.norm[i] * length(log_charges)}
exp.f.norm # Print expected frequencies
obs.f.norm <- as.numeric(log_charges.table) # Observed frequencies
obs.f.norm # Print observed frequencies
x2.norm <- sum((obs.f.norm - exp.f.norm)^2 / exp.f.norm) # Calculate the chi-squared statistic
x2.norm  # Print the chi-squared statistic
pval.norm <- 1 - pchisq(x2.norm, df = 6) # Calculate the p-value using the chi-squared distribution and the degrees of freedom
pval.norm  # Print the p-value

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
B <- 1000 # set the number of bootstrap samples
median.bst <- numeric(B) # Initialize the vector to store the medians of the bootstrap samples
for(i in 1:B){ # Loop over the number of bootstrap samples
  boot.sample <- sample(charges, size = 149, replace = TRUE) # Generate a bootstrap sample with replacement and the same size as the original sample 
  median.bst[i] = median(boot.sample)} # Store the median of each bootstrap sample
summary(median.bst) # Summary statistics of the bootstrap medians
bootstrap_CI = quantile(median.bst, c(0.025, 0.975)) # 95% bootstrap CI, 2.5% and 97.5% quantiles
bootstrap_CI
par(mfrow=c(1,1)) # Histogram of the bootstrap medians
hist(median.bst,
     main = "Histogram of bootstrap medians with 95% CI",
     xlab = "Median cost in 2024 (USD)",
     col  = "darkgreen")
abline(v = bootstrap_CI, lwd = 3, col = 'red') # Add vertical lines for the 95% CI
boxplot(median.bst, # Boxplot of the bootstrap medians
        main = "Boxplot of bootstrap medians",
        col  = "lightgreen",
        horizontal = TRUE)

###
#6#
###
#Normal distribution parameters
mu_log <- 9 # mean
sigma2_log <- 1 # variance 
sigma_log <- sqrt(sigma2_log)  # standard deviation
median_log <- mu_log # median = mean in a normal distribution
n <- 149 # sample size
#m_hat <- exp(mu_log); # Median of the sample in original scale
#m_hat # Print the median of the sample in the original scale
se <- sigma_log * sqrt(pi / (2 * n)); # Standard error of the sample median
se # Print the standard error of the sample median
#95% CI for the population median on the logarithmic scale
median_log - 1.96 * se; # Lower
median_log + 1.96 * se # Upper
#95% CI for the population median on the original scale
exp(median_log - 1.96 * se); # Lower
exp(median_log + 1.96 * se) # Upper

###
#7#
###
B <- 10000 # Number of bootstraps
median.hyp = 8500 # Hypothesized median 8,500 USD
boot.median.H0 = numeric(B) # Initialize the vector to store the medians of the bootstrap samples
for (i in 1:B){
  y.boot = rexp(length(charges), rate = log(2) / median.hyp) 
  boot.median.H0[i] = median(y.boot)} # Store the median of each bootstrap sample
# Histogram of the bootstrap medians and the sample median
hist(boot.median.H0, 
     col="darkgreen", 
     main="Histogram of bootstrap medians and sample median")
m.hat = median(charges) # Sample median
abline(v = m.hat, col="red", lwd=2.0) # Add a vertical line for the sample median
boot.pval = (1 + length(boot.median.H0[boot.median.H0 >= m.hat])) / (B + 1) 
boot.pval # Print the p-value
t.test(charges, mu=median.hyp, alternative="greater") # One-sample t-test