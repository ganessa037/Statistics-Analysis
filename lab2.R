
# Q2

# a)
temp <- c(953,955,948,951,949,954,950,959)
mean_temp <- mean(temp)
print(mean_temp)

sd_temp <- sd(temp)
print(sd_temp)

# b)
md_temp <- median(temp)
print(md_temp)


# Question 3
injury_data <-  read.csv("U:/[1] GANESSA/Code/MMU_CS/Y2T3/Statistics & Analysis/injury.csv")
print(injury_data)

#Bar Chart
library(ggplot2)
ggplot(injury_data, aes(x=Injury_Type)) + geom_bar(fill="red") + labs(title="Bar Chart of Injury Types", x="Injury Type", y="Frequency")

#Pie chart
injury_table <- table(injury_data$Injury_Type)
pie(injury_table, main="Pie Chart of Injury Types", col=rainbow(length((injury_table))))


# Question 4
weight_data <-  read.csv("U:/[1] GANESSA/Code/MMU_CS/Y2T3/Statistics & Analysis/weights.csv")
weights <- weight_data$weight

# a)
print(mean(weights))

# b)
print(round(var(weights),2))

# c)
print(summary(weights))

# d)
print(IQR(weights))

# e)
boxplot(weights, main="Boxplot of Weights", ylab="Weight (grams)", xlab="Automobile Parts")

# f)
hist(weights, main="Histogram of Weights", xlab="Weight (grams", ylab="Frequency", col="lightgreen")


# Question 5
idts_data <-  read.csv("U:/[1] GANESSA/Code/MMU_CS/Y2T3/Statistics & Analysis/IDT.csv")
idts <- idts_data$Interdivision_Time


# a)
hist(idts, main="Histogram of Interdivision Times", xlab="Interdivision Time", ylab="Frequency", col="lightblue")

# b)
log_idts = log10(idts)
print(log_idts)

# c)
hist(log_idts, main="Histogram of log10(IDT)", xlab="Interdivision Time", ylab="Frequency", col="orange")



