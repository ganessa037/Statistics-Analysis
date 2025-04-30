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
print(mean_age)

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

print(quartiles_fw_3)

summary(billionaire_data)


# c)
# Set layout for multiple plots
par(mfrow = c(2, 2))
# Boxplot
boxplot(billionaire_data$age, 
        main = "Boxplot of Billionaire Ages",
        ylab = "Age (Years)")

# Histogram
hist(billionaire_data$age, 
     breaks = 20, 
     col = "skyblue",
     main = "Histogram of Billionaire Ages",
     xlab = "Age (Years)")

# Boxplot
boxplot(billionaire_data$finalWorth, 
        main = "Boxplot of Net Worth",
        ylab = "USD Billion")

# Histogram
hist(billionaire_data$finalWorth, 
     breaks = 20, 
     col = "lightgreen",
     main = "Histogram of Net Worth",
     xlab = "USD Billion")


# Using ggplot

ggplot(billionaire_data, aes(x = age)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Billionaire Ages", x = "Age", y = "Frequency")

ggplot(billionaire_data, aes(x = finalWorth)) +
  geom_histogram(bins = 50, fill = "salmon", color = "black") +
  labs(title = "Distribution of Billionaire Net Worth", x = "Net Worth ($)", y = "Frequency")

# Boxplots
ggplot(billionaire_data, aes(y = age)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot: Billionaire Age")

ggplot(billionaire_data, aes(y = finalWorth)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Boxplot: Net Worth ($)")
#--------------------------------------- END of Part A ---------------------------------------






