billionaire_raw_dataset <-  read.csv("U:/[1] GANESSA/Code/MMU_CS/Y2T3/Statistics & Analysis/Assignment 1/Billionaires Statistics Dataset.csv")
print(billionaire_raw_dataset)


names(billionaire_raw_dataset)
head(billionaire_raw_dataset)
str(billionaire_raw_dataset)
summary(billionaire_raw_dataset)
dim(billionaire_raw_dataset)


#Start from here:
# Select relevant columns
selected_data <- billionaire_raw_dataset[, c("age", "finalWorth", "status")]

# Convert status to factor
selected_data$status <- factor(selected_data$status,
                               levels = c("D", "U"),
                               labels = c("Self-Made", "Inherited"))

# Remove rows where age, finalWorth, or status is NA
billionaire_data <- selected_data[complete.cases(selected_data[, c("age", "finalWorth", "status")]), ]
dim(billionaire_data)

#------------------------------------- ---------------

# Compute summary stats for age
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


# Compute summary stats for finalWorth
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
        ylab = "USD Billion")

# Histogram
hist(billionaire_data$finalWorth, 
     breaks = 20, 
     col = "lightgreen",
     main = "Histogram of Net Worth",
     xlab = "USD Billion",
     ylab = "No of Billionaires")

#--------------------------------------- END of Part A ---------------------------------------


#----------- Start of Part B -------------------#


# Calculate mean and standard deviation
mean_age <- mean(billionaire_data$age)
sd_age <- sd(billionaire_data$age)

# Define intervals based on SD
lower_68 <- mean_age - sd_age
upper_68 <- mean_age + sd_age

lower_95 <- mean_age - 2 * sd_age
upper_95 <- mean_age + 2 * sd_age

lower_997 <- mean_age - 3 * sd_age
upper_997 <- mean_age + 3 * sd_age

# Count how many data points fall within each range
within_68 <- sum(billionaire_data$age >= lower_68 & billionaire_data$age <= upper_68, na.rm = TRUE)
within_95 <- sum(billionaire_data$age >= lower_95 & billionaire_data$age <= upper_95, na.rm = TRUE)
within_997 <- sum(billionaire_data$age >= lower_997 & billionaire_data$age <= upper_997, na.rm = TRUE)

# Calculate proportions (percentages)
total_obs <- nrow(billionaire_data)

prop_within_68 <- (within_68 / total_obs) * 100
prop_within_95 <- (within_95 / total_obs) * 100
prop_within_997 <- (within_997 / total_obs) * 100

# Print results
print(paste("Mean Age:", round(mean_age, 2)))
print(paste("Standard Deviation of Age:", round(sd_age, 2)))
print(paste("Proportion within 1 SD (Expected ~68%):", round(prop_within_68, 2), "%"))
print(paste("Proportion within 2 SDs (Expected ~95%):", round(prop_within_95, 2), "%"))
print(paste("Proportion within 3 SDs (Expected ~99.7%):", round(prop_within_997, 2), "%"))
















