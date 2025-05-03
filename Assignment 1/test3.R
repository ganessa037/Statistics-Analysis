install.packages("viridis")


# Load required libraries
library(ggplot2)

# Step 1: Read the dataset
billionaire_raw_dataset <- read.csv("U:/[1] GANESSA/Code/MMU_CS/Y2T3/Statistics & Analysis/Assignment 1/Billionaires Statistics Dataset.csv")

# Step 2: Select relevant columns including 'country'
selected_data <- billionaire_raw_dataset[, c("age", "finalWorth", "status", "country")]

# Step 3: Convert 'status' to factor
selected_data$status <- factor(selected_data$status,
                               levels = c("D", "U"),
                               labels = c("Self-Made", "Inherited"))

# Step 4: Remove rows with NA in age, finalWorth, status, or country
billionaire_data <- selected_data[complete.cases(selected_data[, c("age", "finalWorth", "status", "country")]), ]

# Optional: Limit to top N countries for clarity (e.g., top 5)
top_countries <- names(sort(table(billionaire_data$country), decreasing = TRUE))[1:5]
billionaire_subset <- subset(billionaire_data, country %in% top_countries)

# If you want **all countries**, comment out the above two lines and use:
# billionaire_subset <- billionaire_data






library(viridis)

ggplot(billionaire_subset, aes(x = age, fill = country)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
  labs(title = "Billionaire Age Distribution by Country", x = "Age", y = "Count", fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_viridis_d()


#-------------------------------------------------------------

ggplot(billionaire_subset, aes(x = age, fill = country)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
  labs(title = "Billionaire Age Distribution by Country", x = "Age", y = "Count", fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_viridis_d(option = "magma")


ggplot(billionaire_subset, aes(x = age, fill = country)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
  labs(title = "Billionaire Age Distribution by Country", x = "Age", y = "Count", fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_viridis_d(option = "plasma")


ggplot(billionaire_subset, aes(x = age, fill = country)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
  labs(title = "Billionaire Age Distribution by Country", x = "Age", y = "Count", fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_viridis_d(option = "plasma")


ggplot(billionaire_subset, aes(x = age, fill = country)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
  labs(title = "Billionaire Age Distribution by Country", x = "Age", y = "Count", fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_viridis_d(option = "inferno")



#---------

# Rainbow Palette
ggplot(billionaire_subset, aes(x = age, fill = country)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
  labs(title = "Billionaire Age Distribution by Country (Rainbow Palette)", x = "Age", y = "Count", fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_manual(values = rainbow(length(unique(billionaire_subset$country))))

# Heat Palette
ggplot(billionaire_subset, aes(x = age, fill = country)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
  labs(title = "Billionaire Age Distribution by Country (Heat Palette)", x = "Age", y = "Count", fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_manual(values = heat.colors(length(unique(billionaire_subset$country))))

# CMYK Palette
ggplot(billionaire_subset, aes(x = age, fill = country)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
  labs(title = "Billionaire Age Distribution by Country (CMYK Palette)", x = "Age", y = "Count", fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_manual(values = cm.colors(length(unique(billionaire_subset$country))))



# Blues Palette
ggplot(billionaire_subset, aes(x = age, fill = country)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
  labs(title = "Billionaire Age Distribution by Country (Blues Palette)", x = "Age", y = "Count", fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_brewer(palette = "Blues")

# Greens Palette
ggplot(billionaire_subset, aes(x = age, fill = country)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
  labs(title = "Billionaire Age Distribution by Country (Greens Palette)", x = "Age", y = "Count", fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_brewer(palette = "YlOrRd")

# Reds Palette
ggplot(billionaire_subset, aes(x = age, fill = country)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
  labs(title = "Billionaire Age Distribution by Country (Reds Palette)", x = "Age", y = "Count", fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_brewer(palette = "RdBu")

# Viridis-like Yellow-Green-Blue Palette
ggplot(billionaire_subset, aes(x = age, fill = country)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 20, color = "black") +
  labs(title = "Billionaire Age Distribution by Country (Yellow-Green-Blue Palette)", x = "Age", y = "Count", fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right") +
  scale_fill_brewer(palette = "Set1")

#---------- BOXPLOT ----------------------
ggplot(billionaire_subset, aes(x = country, y = age, fill = country)) +
  geom_boxplot(color = "black", alpha = 0.7, outlier.color = "red", outlier.size = 2) +
  labs(
    title = "Billionaire Age Distribution by Country (Set1 Palette)",
    x = "Country",
    y = "Age",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.title = element_blank()  # Optional: remove legend title
  ) +
  scale_fill_brewer(palette = "Set1")  # Using the same Set1 palette


#------ violin plot


ggplot(billionaire_subset, aes(x = country, y = age, fill = country)) +
  geom_violin(trim = FALSE, alpha = 0.7) +  # Violin plot
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.title = element_blank()  # Optional: remove legend title
  ) +
  scale_fill_brewer(palette = "Set1")  # Using the same Set1 palette


