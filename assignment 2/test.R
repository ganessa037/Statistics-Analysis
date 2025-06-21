# ---------------------------- Part A ----------------------------
# --- Question 1 ---
old_battery_data <- data.frame(
  midpoint = c(549, 562.5, 577.5, 592.5, 607.5, 622.5, 637.5, 652.5),
  freq       = c(16, 54, 161, 274, 279, 138, 58, 20)
)

# Estimate mean and standard deviation from grouped data
old_battery_data$fx <- old_battery_data$midpoint * old_battery_data$freq
old_battery_data$fx2 <- (old_battery_data$midpoint)^2 * old_battery_data$freq

sum_f <- sum(old_battery_data$freq)
sum_fx <- sum(old_battery_data$fx)
sum_fx2 <- sum(old_battery_data$fx2)

old_mean <- sum_fx / sum_f
old_sd <- sqrt((sum_fx2 - (sum_fx^2 / sum_f)) / (sum_f - 1))

cat("Old Battery Batch:\n")
cat("Estimated Mean =", round(old_mean), "\n")
cat("Estimated Standard Deviation =", round(old_sd), "\n\n")
#--------------

# --- Question 2 ---
new_battery_data <-  read.csv("U:/[1] GANESSA/Code/MMU_CS/Y2T3/Statistics & Analysis/Assignment 2/BatteryLifeNew.csv")

# --- Compute Sample Mean and SD for New Battery ---
new_mean <- mean(new_battery_data$LifetimeHrs)
new_sd <- sd(new_battery_data$LifetimeHrs)
n <- nrow(new_battery_data)

# --- Compute 95% Confidence Interval using t-distribution ---
t_critical <- qt(p = 0.975, df = n - 1)
margin_of_error <- t_critical * (new_sd / sqrt(n))
ci_lower <- new_mean - margin_of_error
ci_upper <- new_mean + margin_of_error

cat("New Battery Batch:\n")
cat("Sample Mean =", round(new_mean, 4), "\n")
cat("Sample Standard Deviation =", round(new_sd, 4), "\n")
cat("95% Confidence Interval: [", round(ci_lower, 4), ",", round(ci_upper, 4), "]\n")

# ---------------------------- Part B ----------------------------
library(ggplot2)

# Histogram
ggplot(new_battery_data, aes(x = LifetimeHrs)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 10, 
                 fill = "thistle", 
                 color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = new_mean, sd = new_sd), 
                color = "red", 
                size = 1) +
  labs(
    title = "Histogram of New Battery Lifetimes",
    x = "Battery Lifetime (hours)",
    y = "Density"
  ) +
  theme_minimal()





# --- Q-Q Plot for Normality ---
qqnorm(new_battery_data$LifetimeHrs, main = "Q-Q Plot of Battery Lifetimes")
qqline(new_battery_data$LifetimeHrs, col = "red", lwd = 2)

# --- Shapiro-Wilk Test for Normality ---
shapiro_test_result <- shapiro.test(new_battery_data$LifetimeHrs)
print(shapiro_test_result)




# ---------------------------- Part C ----------------------------
# --- One-sample t-test to compare new battery mean to 600 hours ---
t_test_result <- t.test(new_battery_data$LifetimeHrs, mu = 600)

# Print results
print(t_test_result)
























