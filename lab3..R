# Lab 3

#Q1
mu <- 3.2
sigma <- 0.5

#a)
lower_68 <- mu - sigma
upper_68 <- mu + sigma
cat("68% of newborn weights are between", lower_68, "kg, and",upper_68)

#b)
lower_95 <- mu - 2*sigma
upper_95 <- mu + 2*sigma
cat("95% of newborn weights are between", lower_95, "kg, and",upper_95,"kg")

#c)
lower_997 <- mu - 3*sigma
upper_997 <- mu + 3*sigma
cat("99.7% of newborn weights are between", lower_997, "kg, and",upper_997,"kg")

#d)
p_below_2.4 <- pnorm(2.4, mean = mu, sd = sigma)
cat("Percentage of newborn weighing less than 2.4kg:", round(p_below_2.4 *100,2),"%\n")

#e)
p_above_4.2 <- pnorm(4.2, mean = mu, sd = sigma, lower.tail = FALSE)
cat("Percentage of newborn weighing less than 2.4kg:", round(p_above_4.2 *100,2),"%\n")

#--------------------------------
#Q2
reaction_times <- c(2.8,3.1,2.5,3.0,2.7,3.4,2.9,3.2,2.6,3.3)

# a)
mean_rt <- mean(reaction_times)
sd_rt <- sd(reaction_times)
print(mean_rt)
print(sd_rt)

cat("68% range:",mean_rt - sd_rt,"to",(mean_rt+sd_rt))
cat("68% range:",round(mean_rt - sd_rt,2),"to",round(mean_rt+sd_rt,2))

# b)
cat("95% range:",round(mean_rt - 2*sd_rt,2),"to",round(mean_rt+2*sd_rt,2))

# c)
cat("99.7% range:",round(mean_rt - 3*sd_rt,2),"to",round(mean_rt+3*sd_rt,2))





#--------------------------------

#Q3
lower_bound <- mean_rt - sd_rt
upper_bound <- mean_rt + sd_rt
within_1sd <- sum(reaction_times > lower_bound & reaction_times < upper_bound)
cat(within_1sd, "out of 10")
#-------------------------
#Q4
#Generate data
set.seed(123)
data <- rnorm(100)

#QQ Plot
qqnorm(data, main = "QQ-Plot of standard normal data")
qqline(data, col = "blue")
#--------------------------------
# Q5

data(mtcars)
mtcars <- as.data.frame(mtcars)
str(mtcars)

qqnorm(mtcars$mpg, main = "QQ-Plot of MPG from mtcars",
       xlab ="Theorectical Quantiles",
       ylab = "sample quantiles")
qqline(mtcars$mpg, col="blue",lwd =2)
shapiro.test((mtcars$mpg))





#--------------------------------
# Q6

data(iris)
print(unique(iris$Species))

#Extract Sepal.length for setosa and versicolor
setosa <- subset












