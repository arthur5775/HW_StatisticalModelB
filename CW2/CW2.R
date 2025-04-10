getwd() # Print the current working directory

###
#1# 
###
# a)
# Read the data from the csv file into R
vehicle_data <- read.csv("vehicle.csv", header = TRUE)
# Numerical summaries
head(vehicle_data) # Display the first few rows of the dataset
str(vehicle_data)  # Check structure of the dataset
summary(vehicle_data) # Summary statistics
nrow(vehicle_data)  # Number of rows = total number of vehicles
# Categorical summaries
#table(vehicle_data$brand) # Count of vehicles by brand
# Frequency table with counts and percentages for vehicle brands
brand_counts <- table(vehicle_data$brand)
brand_percent <- prop.table(brand_counts) * 100
data.frame(Brand = names(brand_counts), Count = as.vector(brand_counts), Percentage = round(as.vector(brand_percent), 2))
#table(vehicle_data$fueltype) # Count of vehicles by fuel type
# Frequency table with counts and percentages
fueltype_counts <- table(vehicle_data$fueltype)
fueltype_percent <- prop.table(fueltype_counts) * 100
data.frame(Fueltype = names(fueltype_counts), Count = as.vector(fueltype_counts), Percentage = round(as.vector(fueltype_percent), 2))
#table(vehicle_data$type) # Count of vehicles by type
# Frequency table with counts and percentages for vehicle types
type_counts <- table(vehicle_data$type)
type_percent <- prop.table(type_counts) * 100
data.frame(Type = names(type_counts), Count = as.vector(type_counts), Percentage = round(as.vector(type_percent), 2))
# Summary for numeric variables
summary(vehicle_data$age)     # Summary statistics for age
summary(vehicle_data$price)   # Summary statistics for price
# Standard Deviation
sd(vehicle_data$age, na.rm = TRUE)    # Standard deviation for age
sd(vehicle_data$price, na.rm = TRUE)  # Standard deviation for price
# Interquartile Range (IQR)
IQR(vehicle_data$age, na.rm = TRUE)   # IQR for age
IQR(vehicle_data$price, na.rm = TRUE) # IQR for price
# Mode function since R doesn't have a built-in one for numerical data
get_mode <- function(v) {
  uniq_vals <- unique(v)
  uniq_vals[which.max(tabulate(match(v, uniq_vals)))]}
get_mode(vehicle_data$age)    # Mode for age
get_mode(vehicle_data$price)  # Mode for price

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
# Boxplot of vehicle number by type
ggplot(vehicle_data, aes(x = type, fill = type)) + geom_bar() +
  labs(title = "Count of Vehicles by Type", x = "Type", y = "Count") + theme_minimal()



# b) 
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
  theme_minimal() + xlim(0, 35)
# Scatter plot of price vs age vs type
ggplot(vehicle_data, aes(x = age, y = price, color = type)) + geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") + labs(title = "Vehicle Age vs Price by Vehicle Type", x = "Vehicle Age (Years)", y = "Price (USD)") +
  theme_minimal() + xlim(0, 35)

# c) 
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

# confidence interval for Pearson’s correlation using bootstrap resampling. 
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
cat("Bootstrap 99% Confidence Interval for Pearson Correlation: [",round(ci_boot$percent[4], 3), ",", round(ci_boot$percent[5], 3), "]\n")



# d) 
# Categorical values
vehicle_data$type <- as.factor(vehicle_data$type)
vehicle_data$brand <- as.factor(vehicle_data$brand)
vehicle_data$fueltype <- as.factor(vehicle_data$fueltype)
# First model
model1 = lm(price ~ age * type + fueltype + brand, data = vehicle_data)
summary(model1)
anova(model1)

#second model
model2 = lm(price ~ age  * type + fueltype, data = vehicle_data)
summary(model2)
anova(model2)
#plotting
std_res <- rstandard(model2)
# 5 largest residuals
top5_residuals <- order(abs(std_res), decreasing = TRUE)[1:5]
# Residuals vs Fitted
plot(model2$fitted.values, model2$residuals,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 1)
abline(h = 0, col = "red", lty = 2)
# Adding the number of the largest point
text(model2$fitted.values[top5_residuals],
     model2$residuals[top5_residuals],
     labels = top_resid,
     cex = 0.6,
     pos = 3, col = "blue")
# Q-Q Residuals
qqnorm(std_res, main = "Q-Q Residuals", pch = 1)
qqline(std_res, col = "gray", lty = 2)
# Adding the number of the largest point
text(x = qqnorm(std_res, plot.it = FALSE)$x[top5_residuals],
     y = std_res[top_resid],
     labels = top_resid,
     cex = 0.6,
     pos = 3, col = "blue")
# Scale location
plot(model2, which = 3)
# Cook's distances
cooks_distance <- cooks.distance(model2)
# 3 most influential points
top3_cooks <- order(cooks_distance, decreasing = TRUE)[1:5]
# Plot Cook's Distance
plot(cooks_distance, type = "h", main = "Cook's Distance", ylab = "Cook's distance", xlab = "Obs number")
text(x = top3_cooks, y = cooks_distance[top3_cooks], labels = top3_cooks, pos = 2, col = "blue", cex = 0.8)
abline(h = 4 / nrow(vehicle_data), col = "red", lty = 2)

# Remove the influential point
print(vehicle_data[39, ])
vehicle_clean <- vehicle_data[c(-39), ]
nrow(vehicle_clean)
# Rebuild the model
model2_clean <- lm(price ~ age * type + fueltype, data = vehicle_clean)
# Summary and anova
summary(model2_clean)
anova(model2_clean)
# Plotting
std_res <- rstandard(model2_clean)
top5_residuals <- order(abs(std_res), decreasing = TRUE)[1:5]
plot(model2_clean$fitted.values, model2_clean$residuals,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 1)
abline(h = 0, col = "red", lty = 2)
text(model2_clean$fitted.values[top5_residuals],
     model2_clean$residuals[top5_residuals],
     labels = top_resid, cex = 0.7, pos = 3, col = "blue")
qqnorm(std_res, main = "Q-Q Residuals", pch = 1)
qqline(std_res, col = "gray", lty = 2)
text(x = qqnorm(std_res, plot.it = FALSE)$x[top5_residuals],
     y = std_res[top5_residuals],
     labels = top_resid,
     cex = 0.7, pos = 3, col = "blue")
plot(model2_clean, which = 3)
cooks_distance <- cooks.distance(model2_clean)
top5_cooks <- order(cooks_distance, decreasing = TRUE)[1:5]
plot(cooks_distance, type = "h",
     main = "Cook's Distance",
     ylab = "Cook's distance",
     col = "red", lwd = 2,
     ylim = c(0, 0.25),
     xlim = c(0, nrow(vehicle_clean)))
abline(h = 4 / nrow(vehicle_clean), col = "blue", lty = 2)
text(x = top5_cooks, y = cooks_distance[top5_cooks],
     labels = top5_cooks, pos = 2, col = "blue", cex = 0.8)



###
#2# 
###
# a)
# Read the data from the csv file into R
airline_data <- read.csv("Airline.csv", header = TRUE, sep=",")

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

# b) 
gender_table <- table(airline_data$Satisfaction, airline_data$Gender)
print(gender_table)
chi_gender <- chisq.test(gender_table)
chi_gender
chi_gender$expected # Expected counts

class_table <- table(airline_data$Satisfaction, airline_data$Class)
print(class_table)
chi_class <- chisq.test(class_table)
chi_class
chi_class$expected # Expected counts

# c) 
# Fit the full model
model <- glm(Satisfaction ~ Gender + Class + Distance + Delay, data = airline_data, family = binomial)
summary(model)
anova(model, test = "Chisq") 
# I choose to retain only the significant variables
reduced_model <- glm(Satisfaction ~ Class + Distance, data = airline_data, family = binomial)
summary(reduced_model)
anova(reduced_model, test = "Chisq") 
# Compare the full and reduced models
anova(model, reduced_model, test = "Chisq")
# Odds ratios are great for interpreting effects (e.g., flying Business Class increases the odds of being satisfied by X times)
exp(coef(reduced_model))

# Standardized Pearson residuals
pearson_resid <- rstandard(reduced_model, type = "pearson")
fitted_vals <- fitted(reduced_model)
# Residuals vs Fitted
plot(fitted_vals, pearson_resid,
     xlab = "Fitted values", ylab = "Pearson Residuals",
     main = "Residuals vs Fitted (Logistic Model)", pch = 1)
abline(h = 0, col = "red", lty = 2)
# Q-Q Plot of residuals
qqnorm(pearson_resid, main = "Q-Q Plot of Pearson Residuals")
qqline(pearson_resid, col = "gray", lty = 2)
# Cook's Distance
cooks_d <- cooks.distance(reduced_model)
plot(cooks_d, type = "h", main = "Cook's Distance", ylab = "Cook's Distance", xlab = "Observation")
abline(h = 4 / nrow(airline_data), col = "red", lty = 2)
top_cooks <- order(cooks_d, decreasing = TRUE)[1:5]
text(x = top_cooks, y = cooks_d[top_cooks], labels = top_cooks, pos = 2, col = "blue", cex = 0.8)

# Remove the influential points
airline_data_clean <- airline_data[-top_cooks, ]
# Rebuild the model
opti_model <- glm(Satisfaction ~ Class + Distance, data = airline_data_clean, family = binomial)
summary(opti_model)
anova(opti_model, test = "Chisq")

# Standardized Pearson residuals
pearson_resid <- rstandard(opti_model, type = "pearson")
fitted_vals <- fitted(opti_model)
# Residuals vs Fitted
plot(fitted_vals, pearson_resid,
     xlab = "Fitted values", ylab = "Pearson Residuals",
     main = "Residuals vs Fitted (Logistic Model)", pch = 1)
abline(h = 0, col = "red", lty = 2)
# Q-Q Plot of residuals
qqnorm(pearson_resid, main = "Q-Q Plot of Pearson Residuals")
qqline(pearson_resid, col = "gray", lty = 2)
# Cook's Distance
cooks_d <- cooks.distance(opti_model)
plot(cooks_d, type = "h", main = "Cook's Distance", ylab = "Cook's Distance", xlab = "Observation")
abline(h = 4 / nrow(airline_data_clean), col = "red", lty = 2)
top_cooks <- order(cooks_d, decreasing = TRUE)[1:5]
text(x = top_cooks, y = cooks_d[top_cooks], labels = top_cooks, pos = 2, col = "blue", cex = 0.8)


# d) 
# Prediction of probabilities using the reduced model
predicted_probs <- predict(reduced_model, type = "response")
# Show the first 10 predicted probabilities
pred <- data.frame(
  Predicted_Probability = round(predicted_probs[1:10], 3),
  Actual_Satisfaction = airline_data$Satisfaction[1:10])
print(pred)
# Show the percentage of satisfied passengers with a threshold of 0.5
pred$Predicted_Label <- ifelse(pred$Predicted_Probability >= 0.5, 1, 0)
pred$Correct <- pred$Predicted_Label == pred$Actual_Satisfaction
mean(pred$Correct)
# We can try to improve the model by modifying the threshold for classification
# It will change depending on the priority of the model (increasing sensitivity/recall or precision)
thresholds <- seq(0.4, 0.6, by = 0.05)
accuracies <- sapply(thresholds, function(t) {
  pred_label <- ifelse(predicted_probs >= t, 1, 0)
  mean(pred_label == airline_data$Satisfaction)})
data.frame(thresholds, accuracies) # The best one seems to be 0.55 with 0.758 accuracy