# 1) Find all numbers between 1 and 2000 that are multiples of 317
multiples_317 <- (1:2000)[(1:2000)%% 317 == 0]
print(multiples_317)

# 2) Count how many numbers between 1 and 2000 are multiples of 17
count_multiples_17 <- sum((1:2000)%% 17 == 0)
print(count_multiples_17)

# 3) Words with less than 6 or more than 8 characters
states <- c("Maine", "Maryland", "Minnesota", "Massachusetts", "Michigan", "Mississippi",
            "Missouri", "Montana")
selected_states <- states[nchar(states) < 6 | nchar(states) > 8]
print(selected_states)

# 4) Function to return product of min and max in a vector (8,17,21,2,-9,0)
min_max_product <- function(x) {return(min(x)*max(x))}
numbers <- c(8,17,21,2,-9,0)
result <- min_max_product(numbers)
print(result)

# 5) Function to convert Celcius to Farenheit
c_to_f <- function(c) {return((9/5)*c + 32)}
temp_f <- c_to_f(32)
print(temp_f)

# 6) Functions to find all numbers

find_multiples <- function(x,y) {return (x[x %% y ==0])}
test <- 1:2000
multiples_of_100 <- find_multiples (test, 100)
print(multiples_of_100)





