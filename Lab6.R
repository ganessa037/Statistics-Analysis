#q1
mean_weight <- 3.14
sd_weight <- 2.4
n <- 26
alpha <- 0.01

error_margin <- qnorm(1-alpha/2)*sd_weight/sqrt(n)
lower_bound <- mean_weight - error_margin
print(lower_bound)


upper_bound <- mean_weight + error_margin
print(upper_bound)

#Q2
n_new <- 100
error_margin <- qnorm(1-alpha/2)*sd_weight/sqrt(n_new)
lower_bound_new <- mean_weight - error_margin
print(lower_bound_new)


upper_bound_new <- mean_weight + error_margin
print(upper_bound_new)


sd_new <- 1
error_margin_sd1 <- qnorm(1-alpha/2)*sd_new/sqrt(n)
lower_bound_sd1 <- mean_weight - error_margin_sd1
print(lower_bound_sd1)

upper_bound_sd1 <- mean_weight + error_margin_sd1
print(upper_bound_sd1)

alpha_95 <- 0.05
error_margin_95 <- qnorm(1-alpha_95/2)*sd_weight/sqrt(n)
lower_bound_95 <- mean_weight - error_margin_95
print(lower_bound_95)


upper_bound_95 <- mean_weight + error_margin_95
print(upper_bound_95)


#q3
mean_current <- 280.3
sd_current <- 10.3
n_bulb <- 25
alpha_90 <- 0.10


error_margin_current <- qt(1-alpha_90/2,
                           df=n_bulb-1)*sd_current/sqrt(n_bulb)
lower_bound_current <- mean_current - error_margin_current
print(lower_bound_current)




sd_minute <- 12
E <- 5
alpha_90 <- 0.10

z_score <- qnorm(1-alpha_90/2)
n_required <- ceiling((z_score*sd_minute/E)^2)
print(n_required)


weight <- c(218, 207, 219, 200, 205, 221, 206, 205, 211)
mean_weight <- mean(weight)
sd_population <- 7
n_weight <- 9
alpha_98 <- 0.02

error_margin_weight <- qnorm(1-alpha_98)*sd_population/sqrt(n_weight)
upper_bound_98 <- mean_weight + error_margin_weight
print(upper_bound_98)





