# Load required libraries
library(ggplot2)
library(dplyr)

# Step 1: Read and clean the dataset
billionaire_raw_dataset <- read.csv("U:/[1] GANESSA/Code/MMU_CS/Y2T3/Statistics & Analysis/Assignment 1/Billionaires Statistics Dataset.csv")

# Select relevant columns
selected_data <- billionaire_raw_dataset[, c("age", "finalWorth", "status", "country")]

# Convert 'status' to factor
selected_data$status <- factor(selected_data$status,
                               levels = c("D", "U"),
                               labels = c("Self-Made", "Inherited"))

# Remove rows with NA values
billionaire_data <- selected_data[complete.cases(selected_data), ]

ggplot(billionaire_data, aes(x = age, fill = country)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
  labs(title = "Billionaire Age Distribution by Country", x = "Age", y = "Count", fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_viridis_d()
