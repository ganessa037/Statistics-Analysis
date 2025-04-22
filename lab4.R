
#Question 1
data <- read.csv("U:/[1] GANESSA/Code/MMU_CS/Y2T3/Statistics & Analysis/Q1.csv")

X1 <- data$X1
hist(X1, main  ="Histogram for X1", xlab="X1", ylab="Frequency", col="skyblue")
boxplot(X1, main="Boxplot of X1", ylab="variable of X1")
qqnorm(X1)
qqline(X1, col="red")


X2 <- data$X2
hist(X2, main  ="Histogram for X2", xlab="X2", ylab="Frequency", col="skyblue")
boxplot(X2, main="Boxplot of X2", ylab="variable of X2")
qqnorm(X2)
qqline(X2, col="red")


X3 <- data$X3
hist(X3, main  ="Histogram for X3", xlab="X3", ylab="Frequency", col="green")
boxplot(X3, main="Boxplot of X3", ylab="variable of X3")
qqnorm(X3)
qqline(X3, col="red")


X4 <- data$X4
hist(X4, main  ="Histogram for X4", xlab="X4", ylab="Frequency", col="green")
boxplot(X4, main="Boxplot of X4", ylab="variable of X4")
qqnorm(X4)
qqline(X4, col="red")


#Question 2

pulse <- read.csv("U:/[1] GANESSA/Code/MMU_CS/Y2T3/Statistics & Analysis/pulse.csv")

hist(pulse$Pulse1, main="Histogram of Resting Pulse",xlab="Resting Pulse (Pulse 1)", ylab="Frequency",col="skyblue")

summary_male <- summary(pulse$Pulse1[pulse$Gender =="M"])
print(summary_male)

summary_female <- summary(pulse$Pulse1[pulse$Gender =="F"])
print(summary_female)



boxplot(Pulse1 ~ Gender, data = pulse,
        main = "Comparative Boxplot of Resting Pulse by Gender",
        xlab = "Gender", ylab = "Resting Pulse (Pulse 1)",
        col = c("pink","lightblue"),
        horizontal = FALSE)


aggregate(Pulse1~Gender, data=pulse, FUN=function(x) c(mean=mean(x), median=median(x), sd=sd(x)))



#Question 3

bp <- read.csv("U:/[1] GANESSA/Code/MMU_CS/Y2T3/Statistics & Analysis/bp.csv")

hist(bp$Age, main = "Histogram of Age", xlab="Age", ylab="Frequency", col="skyblue")


  bp$BP_Group <- ifelse(
  bp$Systolic_BP >= 90 & bp$Systolic_BP <= 139 &
  bp$Diastolic_BP >= 60 & bp$Diastolic_BP <= 89, "NORMAL",
  ifelse(bp$Systolic_BP > 139 | bp$Diastolic_BP > 89, "HIGH","LOW"))
           
  boxplot(BMI ~ BP_Group, data = bp,
         main = "BMI by Blood Pressure Group",
         xlab = "Blood Pressure Group",
         ylab = "BMI",
         col = c("lightgreen", "skyblue", "pink"))





ggplot(bp, aes(x = Systolic_BP, y = Diastolic_BP)) +
  geom_point(color = "skyblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Systolic vs Diastolic BP",
       x = "Systolic BP", y = "Diastolic BP") +
  theme_minimal()


cor_sd <- cor(bp$Systolic_BP, bp$Diastolic_BP)
print(round(cor_sd,2))





ggplot(bp, aes(x = Systolic_BP, y = BMI)) +
  geom_point(color = "skyblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Systolic vs BMI",
       x = "Systolic BP", y = "BMI") +
  theme_minimal()

cor_sd <- cor(bp$Systolic_BP, bp$BMI)
print(round(cor_sd,2))



