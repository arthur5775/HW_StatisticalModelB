getwd() # Print the current working directory

###
#1# 
###
# The dataset vehicle.csv, available in the Assessed Project 2 folder on Canvas, contains a
# sample of 200 observations on various vehicle models collected in 2022. You can load the data
# into R using a command like read.csv('C:/R/vehicle.csv', header=TRUE). Note that you
# will need to specify your own path to the file. Several variables were recorded for each vehicle,
# but the dataset retains the following:
# • brand: Vehicle brand (Toyota, Honda, Ford, Chevrolet, Nissan).
# • age: Vehicle age (as of 2022).
# • fueltype: Fuel type (diesel, gas, other).
# • price: Vehicle price in USD.
# • type: Vehicle type (pickup, sedan, SUV).

# a) Present numerical and graphical summaries to explore the distributions of each variable:
# brand, age, fueltype, price, and type. Discuss any interesting findings or trends observed from these summaries. [4 marks]

# Read the data from the csv file into R
file_path <- "C:/Users/Framework_Arthur/Downloads/HWU/F79MB_Statistical Model B/CW2/vehicle.csv"
vehicle_data <- read.csv(file_path, header = TRUE)

# Numerical summaries
head (vehicle_data) # Display the first few rows of the dataset
str(vehicle_data) # Check structure of the dataset
summary(vehicle_data) # Summary statistics
nrow(vehicle_data) # Number of rows in the dataset = total number of vehicules
table(vehicle_data$brand) # Count of vehicles by brand
table(vehicle_data$fueltype) # Count of vehicles by fuel type
table(vehicle_data$type) # Count of vehicles by type
summary(vehicle_data$age) # Summary statistics for age
summary(vehicle_data$price) # Summary statistics for price

# Graphical summaries
library(ggplot2)
# Histogram of vehicle prices
ggplot(vehicle_data, aes(x = price)) +  geom_histogram(binwidth = 2000, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Vehicle Prices", x = "Price (USD)", y = "Count") +  theme_minimal()
# Boxplot of vehicle number by fuel type
ggplot(vehicle_data, aes(x = brand, fill = brand)) + geom_bar() +
  labs(title = "Count of Vehicles by Brand", x = "Brand", y = "Count") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Distribution of vehicle age
ggplot(vehicle_data, aes(x = age)) + geom_histogram(binwidth = 2, fill = "blue", alpha = 0.7, color = "black") +
  labs(title = "Distribution of Vehicle Age", x = "Age (Years)", y = "Count") + theme_minimal()
# Boxplot of vehicle number by fuel type
ggplot(vehicle_data, aes(x = fueltype, fill = fueltype)) + geom_bar() +
  labs(title = "Count of Vehicles by Fuel Type", x = "Fuel Type", y = "Count") + theme_minimal()

# b) Produce visualizations to show the relationships between vehicle price (price) and each
# explanatory variable: brand, age, fueltype, and type. Comment on any trends or
# associations you observe. [3 marks]

# Boxplot: Price distribution by brand
ggplot(vehicle_data, aes(x = brand, y = price, fill = brand)) +
  geom_boxplot() + labs(title = "Price Distribution by Brand", x = "Brand", y = "Price (USD)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Scatter plot: Age vs Price
ggplot(vehicle_data, aes(x = age, y = price, color = brand)) +
  geom_point(alpha = 0.6) + geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Vehicle Age vs Price", x = "Vehicle Age (Years)", y = "Price (USD)") + theme_minimal()
# Boxplot: Price distribution by fuel type
ggplot(vehicle_data, aes(x = fueltype, y = price, fill = fueltype)) + geom_boxplot() + 
  labs(title = "Price Distribution by Fuel Type", x = "Fuel Type", y = "Price (USD)") + theme_minimal()
# Boxplot: Price distribution by vehicle type
ggplot(vehicle_data, aes(x = type, y = price, fill = type)) + geom_boxplot() +
  labs(title = "Price Distribution by Vehicle Type", x = "Vehicle Type", y = "Price (USD)") + theme_minimal()
# Scatter plot of price vs age vs fuel type
ggplot(vehicle_data, aes(x = age, y = price, color = fueltype)) + geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") + labs(title = "Vehicle Age vs Price by Fuel Type", x = "Vehicle Age (Years)", y = "Price (USD)") +
  theme_minimal() #+ xlim(0, 35)
# Scatter plot of price vs age vs type
ggplot(vehicle_data, aes(x = age, y = price, color = type)) + geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") + labs(title = "Vehicle Age vs Price by Vehicle Type", x = "Vehicle Age (Years)", y = "Price (USD)") +
  theme_minimal() #+ xlim(0, 35)

# c) Compute a 99% confidence interval for the Pearson correlation between price and age
# using Fisher’s transformation. Comment on your results, including any concerns regarding
# the validity or reliability of the confidence interval. [3 marks]

# Compute Pearson correlation between price and age
correlation <- cor(vehicle_data$price, vehicle_data$age, method = "pearson")
# Sample size (n)
n <- nrow(vehicle_data)
# Fisher’s Z-transformation
z <- 0.5 * log((1 + correlation) / (1 - correlation))
# Standard error of Z
se_z <- 1 / sqrt(n - 3)
# 99% Confidence Interval for Z
z_critical <- qnorm(0.995)  # For 99% CI, use 0.995 (two-tailed)
z_lower <- z - z_critical * se_z
z_upper <- z + z_critical * se_z
# Convert back to correlation scale
r_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
r_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
# Display results
cat("Pearson Correlation between Price and Age:", round(correlation, 3), "\n")
cat("99% Confidence Interval for Correlation: [", round(r_lower, 3), ",", round(r_upper, 3), "]\n")

# Suggest an alternative approach to compute the confidence interval and justify your choice. [1 mark]

# Instead of using Fisher’s transformation, we can compute the confidence interval for Pearson’s correlation using bootstrap resampling. 
# This method does not rely on the assumption of normality and is more robust to outliers and non-linearity.

# Load necessary libraries
library(boot)
# Define function to compute Pearson correlation
correlation_fn <- function(data, indices) {
  sample_data <- data[indices, ]  # Resample data
  return(cor(sample_data$price, sample_data$age, method = "pearson"))}
# Perform bootstrap with 10,000 resamples
set.seed(123)  # For reproducibility
bootstrap_results <- boot(data = vehicle_data, statistic = correlation_fn, R = 10000)
# Compute 99% Confidence Interval (Percentile Method)
ci_boot <- boot.ci(bootstrap_results, type = "perc", conf = 0.99)
# Display results
cat("Bootstrap 99% Confidence Interval for Pearson Correlation: [",
    round(ci_boot$percent[4], 3), ",", round(ci_boot$percent[5], 3), "]\n")

# d) It is proposed to fit a multiple linear regression model with price as the response variable
# and age, type, fueltype, and brand as explanatory variables, including the interaction
# between age and type, but excluding interactions with fueltype and brand. The pro-
# posed model is specified as follows:
# price ∼ age ∗ type + fueltype + brand
# Carry out the appropriate analyses for this model to determine which terms should be retained.
# Identify any influential data points and thoroughly assess the impact of removing
# them from the analysis. Your report should include relevant R outputs, plots, comments,
# model diagnostics, justification for removing the influential points, comprehensive conclusions, and suggestions for further refinement of the fitted model. [14 marks]



# 2) The dataset in the Airline.csv file, available in the Assessed Project 2 folder on Canvas,
# contains a sample of 95 airline passenger satisfaction responses from 2024 for a UK-based air-
# line company. This study aims to forecast the likelihood of passenger satisfaction using an appropriate logistic regression model.
# You can load the data into R using a command like read.table("C:/R/Airline.csv", header=T, sep=","). Note that you will need to specify your own path to the file. The variables given
# are: 
# Gender = Passenger’s gender (Female, Male).
# Class = Travel class (Business, Economy, Economy Plus).
# Distance = Travel distance in kilometers (km).
# Delay = Flight departure delay in minutes.
# Satisfaction = Passenger satisfaction (0 = Neutral or Dissatisfied, 1 = Satisfied).
# Note that Gender, Class and Satisfaction are categorical variables.
# It is proposed to model the data in a generalized linear model framework, modelling Satisfaction
# as the response variable with Gender, Class, Distance and Delay as explanatory variables.

# a) Produce appropriate plots to explore the relationship between the response variable,
# Satisfaction, and the explanatory variables Class and Distance. Comment on your results for each plot. [3 marks]

# Read the data from the csv file into R
file_path <- "C:/Users/Framework_Arthur/Downloads/HWU/F79MB_Statistical Model B/CW2/Airline.csv"
airline_data <- read.csv(file_path, header = TRUE, sep=",")

# Numerical summaries
head (airline_data) # Display the first few rows of the dataset
str(airline_data) # Check structure of the dataset
summary(airline_data) # Summary statistics
nrow(airline_data) # Number of rows in the dataset = total number of passengers
table(airline_data$Gender) # Count the number of male and female passengers
table(airline_data$Class) # Count the number of passengers by class
table(airline_data$Satisfaction) # Count the number of satisfied and unsatisfied passengers
summary(airline_data$Distance) # Summary statistics for distance
summary(airline_data$Delay) # Summary statistics for delay
summary(airline_data$Age) # Summary statistics for age

# Graphical summaries
library(ggplot2)
# Bar plot of satisfaction by class
ggplot(airline_data, aes(x = Class, fill = factor(Satisfaction))) +
  geom_bar(position = "fill") + labs(title = "Satisfaction by Class", x = "Class", y = "Proportion") +
  scale_fill_manual(values = c("red", "green"), labels = c("Neutral/Dissatisfied", "Satisfied")) + theme_minimal()
# Boxplot of satisfaction by distance
ggplot(airline_data, aes(x = factor(Satisfaction), y = Distance, fill = factor(Satisfaction))) +
  geom_boxplot() + labs(title = "Distance by Satisfaction", x = "Satisfaction", y = "Distance (km)") +
  scale_fill_manual(values = c("red", "green"), labels = c("Neutral/Dissatisfied", "Satisfied")) + theme_minimal()
# Scatter plot of distance vs delay colored by satisfaction
ggplot(airline_data, aes(x = Distance, y = Delay, color = factor(Satisfaction))) +
  geom_point(alpha = 0.6) + labs(title = "Distance vs Delay by Satisfaction", x = "Distance (km)", y = "Delay (minutes)") +
  scale_color_manual(values = c("red", "green"), labels = c("Neutral/Dissatisfied", "Satisfied")) + theme_minimal()
# Box plot of delay by satisfaction
ggplot(airline_data, aes(x = factor(Satisfaction), y = Delay, fill = factor(Satisfaction))) +
  geom_boxplot() + labs(title = "Delay by Satisfaction", x = "Satisfaction", y = "Delay (minutes)") +
  scale_fill_manual(values = c("red", "green"), labels = c("Neutral/Dissatisfied", "Satisfied")) + theme_minimal()

# b) Carry out appropriate tests of independence to determine whether the satisfaction status
# (Satisfaction) for the flight is associated with gender (Gender) and travel class (Class).
# Your report should include contingency tables, justification for the tests, p-values and clear conclusions. [6 marks]

gender_table <- table(airline_data$Satisfaction, airline_data$Gender)
print(gender_table)
chi_gender <- chisq.test(gender_table)
chi_gender
class_table <- table(airline_data$Satisfaction, airline_data$Class)
print(class_table)
chi_class <- chisq.test(class_table)
chi_class

# c) Analyse the data by fitting a generalized linear model with Satisfaction as the (Binomial)
# response variable and Gender, Class, Distance, and Delay as explanatory variables
# (with no interaction term). Carry out appropriate analyses to determine which terms
# should be retained in the model. Your analysis should include relevant R output, plots,
# comments, model checking and comprehensive conclusions. [12 marks]



# d) Based on the first 10 predicted probabilities, comment on the accuracy of the preferred
# fitted model’s predictions as discussed in part 2(c). Additionally, discuss potential im-
# provements or enhancements that could be made to the model. [4 marks]



# [Overall presentation: 10 marks]
# [Project total: 60 marks]

