#question 2
mean_bmi <- 27
sd_bmi <- 7.5
sample_size <- 100

range997 <- c(mean_bmi - 3*sd_bmi, mean_bmi + 3*sd_bmi)
print(range997)

SE_sample_mean <- sd_bmi / sqrt(sample_size)
print(SE_sample_mean)

range_sample_mean <- c(mean_bmi - 3*SE_sample_mean,
                         mean_bmi + 3*SE_sample_mean)
print(range_sample_mean)


#question 3
pnorm(2)
1- pnorm(1.8)
qnorm(0.3)

qnorm(0.29, lower.tail = FALSE)

probability <- 0.7 + pnorm(-0.9)
qnorm(probability)


# Question 4
1-pnorm(224, mean=200,sd=15)


pnorm(209, mean=200, sd=15) - pnorm(191, mean=200, sd=15)


x <- 1-pnorm(230, mean=200, sd=15)
x*1000

qnorm(0.25, mean=200, sd=15)



#Question 5
population_mean <- 174.5
population_sd <- 6.9
sample_size <- 25
num_sample <- 200

sampling_sd <- population_sd / sqrt(sample_size)
print(sampling_sd)

sampling_mean <- population_mean
print(sampling_mean)

z1 <- (172.5 - sampling_mean) / sampling_sd
z2 <- (175.8 - sampling_mean) / sampling_sd
y <- pnorm(z2) - pnorm(z1)
y*num_sample


z3 <- (172.0 - sampling_mean) / sampling_sd
pnorm(z3) * num_sample




