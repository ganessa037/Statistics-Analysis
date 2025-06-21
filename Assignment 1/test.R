#billionaire_raw_dataset <-  read.csv("U:/[1] GANESSA/Code/MMU_CS/Y2T3/Statistics & Analysis/Assignment 1/Billionaires Statistics Dataset.csv")
billionaire_raw_dataset <-  read.csv("D:/[1] GanessaWork/degree_Computer_Science/2nd Year 3rd Sem/Statistics-Analysis/Assignment 1/Billionaires Statistics Dataset.csv")

# Select relevant columns
selected_data <- billionaire_raw_dataset[, c("age", "finalWorth", "status")]

# Convert status to factor
selected_data$status <- factor(selected_data$status,
                               levels = c("D", "U"),
                               labels = c("Self-Made", "Inherited"))

# Remove rows where age, finalWorth, or status is NA
billionaire_data <- selected_data[complete.cases(selected_data[, c("age", "finalWorth", "status")]), ]
#------------------------------------- ---------------

# Summary for age
mean_age <- mean(billionaire_data$age)
median_age <- median(billionaire_data$age)
mode_age <- which.max(table(billionaire_data$age))
sd_age <- sd(billionaire_data$age)
var_age <- var(billionaire_data$age)
range_age <- max(billionaire_data$age) - min(billionaire_data$age)
quartiles_age_1 <- quantile(billionaire_data$age, probs = c(0.25))
quartiles_age_2 <- quantile(billionaire_data$age, probs = c(0.5))
quartiles_age_3 <- quantile(billionaire_data$age, probs = c(0.75))
quartile_age <- quantile(billionaire_data$age, probs = c(0.25, 0.5, 0.75))
iqr_age <- IQR(billionaire_data$age)
cv_age <- sd_age / mean_age
cv_age_percentage= cv_age*100
cv_age_percentage=round(cv_age_percentage,2)
cv_age_percentage


print(mean_age)
print(median_age)
print(mode_age)
print(sd_age)
print(var_age)
print(range_age)
print(quartiles_age_1)
print(quartiles_age_2)
print(quartiles_age_3)
print(quartile_age)
print(iqr_age)
print(cv_age)
summary(billionaire_data)


# Summary for finalWorth
mean_fw <- mean(billionaire_data$finalWorth)
median_fw <- median(billionaire_data$finalWorth)
mode_fw <- which.max(table(billionaire_data$finalWorth))
sd_fw <- sd(billionaire_data$finalWorth)
var_fw <- var(billionaire_data$finalWorth)
range_fw <- max(billionaire_data$finalWorth) - min(billionaire_data$finalWorth)
quartiles_fw_1 <- quantile(billionaire_data$finalWorth, probs = c(0.25))
quartiles_fw_2 <- quantile(billionaire_data$finalWorth, probs = c(0.5))
quartiles_fw_3 <- quantile(billionaire_data$finalWorth, probs = c(0.75))
quartile_fw <- quantile(billionaire_data$finalWorth, probs = c(0.25, 0.5, 0.75))
iqr_fw <- IQR(billionaire_data$finalWorth)
cv_fw <- sd_fw / mean_fw
cv_fw_percentage= cv_fw*100
cv_fw_percentage=round(cv_fw_percentage,2)
cv_fw_percentage

max(billionaire_data$finalWorth)


print(mean_fw)
print(median_fw)
print(mode_fw)
print(sd_fw)
print(var_fw)
print(range_fw)
print(quartiles_fw_1)
print(quartiles_fw_2)
print(quartiles_fw_3)
print(quartile_fw)
print(iqr_fw)
print(cv_fw)

summary(billionaire_data)




#--------------------------------------------------------------------------------------------

# Boxplot
boxplot(billionaire_data$age, 
        main = "Boxplot of Billionaire Ages",
        ylab = "Age (Years)")

# Histogram
hist(billionaire_data$age, 
     breaks = 20, 
     col = "skyblue",
     main = "Histogram of Billionaire Ages",
     xlab = "Age (Years)",
     ylab = "No of Billionaires")

# Boxplot
boxplot(billionaire_data$finalWorth, 
        main = "Boxplot of Net Worth",
        ylab = "USD ($ Millions)")

# Histogram
hist(billionaire_data$finalWorth, 
     breaks = 20, 
     col = "lightgreen",
     main = "Histogram of Net Worth",
     xlab = "USD ($ Millions)",
     ylab = "No of Billionaires")


#--------------------------------------- END of Part A ---------------------------------------


#----------- Start of Part B -------------------#


# Get mean and standard deviation of Age
mean_age <- mean(billionaire_data$age)
sd_age <- sd(billionaire_data$age)

# Calculate boundaries for 1, 2, and 3 standard deviations
lower_68 <- mean_age - sd_age
upper_68 <- mean_age + sd_age

lower_95 <- mean_age - 2 * sd_age
upper_95 <- mean_age + 2 * sd_age

lower_997 <- mean_age - 3 * sd_age
upper_997 <- mean_age + 3 * sd_age

# Count how many data points fall within each range
within_68 <- subset(billionaire_data, age > lower_68 & age < upper_68)
within_95 <- subset(billionaire_data, age > lower_95 & age < upper_95)
within_997 <- subset(billionaire_data, age > lower_997 & age < upper_997)


len_row<- length(billionaire_data$age)
prop_1sd <- nrow(within_68) / len_row
prop_2sd <- nrow(within_95) / len_row
prop_3sd <- nrow(within_997) / len_row

# Print results as percentages
print(paste("Percentage within ±1 SD: ", round(prop_1sd * 100, 2), "%"))
print(paste("Percentage within ±2 SD: ", round(prop_2sd * 100, 2), "%"))
print(paste("Percentage within ±3 SD: ", round(prop_3sd * 100, 2), "%"))


#q2
# QQ-plot for Age
qqnorm(billionaire_data$age, main = "QQ Plot for Billionaire Age")
qqline(billionaire_data$age, col = "red")  # Add reference line
shapiro.test(billionaire_data$age)

#q3:
# Scatter plot
plot(billionaire_data$age, billionaire_data$finalWorth,
     main = "Age vs Net Worth of Billionaires",
     xlab = "Age",
     ylab = "Net Worth ($ Millions)",
     pch = 19,
     col = "steelblue")

# regression line
abline(lm(finalWorth ~ age, data = billionaire_data), col = "red", lwd = 2)

# 2. Calculate Pearson correlation coefficient
correlation <- cor(billionaire_data$age, billionaire_data$finalWorth, method = "pearson")
r2_percentage <- correlation * 100
print(paste("Pearson Correlation Coefficient (r):", round(correlation, 3)))
print(paste("Percentage (%):", round(r2_percentage, 3)))




# Extract 'age' and 'net_worth' variables
age <- billionaire_data$age
net_worth <- billionaire_data$finalWorth

# Calculate means
mean_age <- mean(age) 
mean_net_worth <- mean(net_worth)

# Calculate Sxx, Syy, and Sxy
Sxx <- sum((age - mean_age)^2)  # Sum of squared deviations of age
Syy <- sum((net_worth - mean_net_worth)^2)  # Sum of squared deviations of net worth
Sxy <- sum((age - mean_age) * (net_worth - mean_net_worth))  # Sum of cross-products

# Compute Pearson correlation coefficient (r)
r <- Sxy / sqrt(Sxx * Syy)

# Compute R-squared (coefficient of determination)
r_squared <- r^2

# Print results
print(paste("Pearson Correlation Coefficient (r):", round(r, 3)))
print(paste("R-Squared (Proportion of Variance Explained):", round(r_squared, 3)))
print(paste("Percentage (%):", round(r_squared*100, 1)))


