temp <- c(953,955,948,951,949,954,950,959)
mean_temp <- mean(temp)
print(mean_temp)

sd_temp <- sd(temp)
print(sd_temp)

md_temp <- median(temp)
print(md_temp)


# Question 2
injury_data <-  read.csv("U:/[1] GANESSA/Code/Statistics & Analysis/injury.csv")
print(injury_data)

#Bar Chart
library(ggplot2)
ggplot(injury_data, aes(x=Injury_Type)) + geom_bar(fill="green") + labs(title="Bar Chart of Injury Types", x="Injury Type", y="Frequency")

#pie chart
injury_table <- table(injury_data$Injury_Type)
pie(injury_table, main="Pie Chart of Injury Types", col=rainbow(length((injury_table))))


# Q3
weight_data <-  read.csv("U:/[1] GANESSA/Code/Statistics & Analysis/weights.csv")
weights <- weight_data$weight


print(mean(weights))
print(round(var(weights),2))

print(summary(weights))
print(IQR(weights))

# e)
boxplot(weights, main="Boxplot of Weights", ylab="Weight (grams)", xlab="Automobile Parts")

hist(weights, main="Histogram of Weights", xlab="Weight (grams", ylab="Frequency", col="lightgreen")


# q5
idts_data <-  read.csv("U:/[1] GANESSA/Code/Statistics & Analysis/weights.csv")
idts <- idts_data$Interdivision_Time









