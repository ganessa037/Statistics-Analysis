# Read the dataset
fizz_data <- read.csv("FreshFizz.csv")

# Convert Date column to Date type
fizz_data$Date <- as.Date(fizz_data$Date)

# period column to distinguish pre-launch and post-launch
fizz_data$Period <- ifelse(fizz_data$Date < as.Date("2024-06-01"),
                           "Before June",
                           "After June")

ggplot(fizz_data, aes(x = Date, y = Total_Sales)) +
  geom_line(color = "purple") +
  labs(title = "Daily Total Sales Over Time",
       x = "Date",
       y = "Total Sales ($)") +
  theme_minimal()

#------ Question 2-----------

pre_launch <- subset(fizz_data, Date < as.Date("2024-06-01"))
mean_post <- mean(post_launch$Total_Sales)
sd_post <- sd(post_launch$Total_Sales)
cat("Pre-June Sales:\n")
cat("Mean =", (mean_pre), "\n")
cat("SD =", (sd_pre), "\n\n")


post_launch <- subset(fizz_data, Date >= as.Date("2024-06-01"))
mean_pre <- mean(pre_launch$Total_Sales)
sd_pre <- sd(pre_launch$Total_Sales)
cat("Post-June Sales:\n")
cat("Mean =", (mean_post), "\n")
cat("SD =", (sd_post), "\n\n")


box_data <- data.frame(
  Sales = c(pre_launch$Total_Sales, post_launch$Total_Sales),
  Period = c(rep("Before June", nrow(pre_launch)), rep("After June", nrow(post_launch)))
)

# boxplot
boxplot(Sales ~ Period, data = box_data,
        main = "Daily Total Sales Before and After June 1",
        xlab = "Period",
        ylab = "Total Sales ($)",
        col = c("wheat", "thistle"),
        border = "black")

#------ Part B -----------

anova_result <- aov(Sales ~ Period, data = box_data)
anova_summary <- summary(anova_result)
print(anova_summary)

# p-value
p_value <- anova_summary[[1]][["Pr(>F)"]][1]
cat("p-value:", (p_value), "\n")


# Interpretation
if (anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
  cat("There is a statistically significant difference in sales before and after June 1.\n")
} else {
  cat("There is no significant difference in sales before and after June 1.\n")
}

#--------- Part C------------
# sales data for Cola Classic, Lemon Light, and Berry Blast
sales <- c(post_launch$Cola_Classic,
           post_launch$Lemon_Light,
           post_launch$Berry_Blast)

# flavor for each sale
flavor <- c(
  rep("Cola Classic", nrow(post_launch)),
  rep("Lemon Light", nrow(post_launch)),
  rep("Berry Blast", nrow(post_launch))
)

# Combine into a new data frame
flavor_data <- data.frame(Flavor = flavor, Sales = sales)

# Perform One-Way ANOVA
anova_result <- aov(Sales ~ Flavor, data = flavor_data)
anova_summary <- summary(anova_result)
print(anova_summary)

#--------- Part C------------



fizz_data$Date <- as.Date(fizz_data$Date)

post_launch <- subset(fizz_data, Date >= as.Date("2024-06-01"))
correlation_test <- cor.test(
  post_launch$Mango_Zing,
  post_launch$Total_Sales,
  method = "pearson"
)
print(correlation_test)















