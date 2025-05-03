billionaire_raw_dataset <-  read.csv("U:/[1] GANESSA/Code/MMU_CS/Y2T3/Statistics & Analysis/Assignment 1/Billionaires Statistics Dataset.csv")
print(billionaire_raw_dataset)

#Start from here:
# Select relevant columns
selected_data <- billionaire_raw_dataset[, c("age", "finalWorth", "status","country")]
print(selected_data)


# Convert status to factor
selected_data$status <- factor(selected_data$status,
                               levels = c("D", "U"),
                               labels = c("Self-Made", "Inherited"))

# Remove rows where age, finalWorth, or status is NA
billionaire_data <- selected_data[complete.cases(selected_data[, c("age", "finalWorth", "status")]), ]
dim(billionaire_data)

#------------------------------------- ---------------


# Using ggplot

ggplot(billionaire_data, aes(x = age)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Billionaire Ages", x = "Age", y = "Frequency")

#--------------------------------------- END of Part A ---------------------------------------


ggplot(billionaire_data, aes(x = age, y = Frequency, fill = Country)) +
  geom_col()

















