### ADD REFERENCE
### Global Description of Behaviour


# Packages and Data -------------------------------------------------------

# Packages
library(tidyverse)

# Data
data <- read.csv("mating_data.csv")
unmated.df <- data %>% filter(female.mated.before==FALSE)


# Descriptive Statistics --------------------------------------------------

variables <- c("nb.courtships", "nb.mounts", "nb.flees")
summary_stats <- sapply(variables, function(col) {
  values <- data[[col]]
  n <- sum(!is.na(values))  # Exclude NA values
  mean_val <- mean(values, na.rm = TRUE)
  sem_val <- sd(values, na.rm = TRUE) / sqrt(n)
  c(mean = mean_val, SEM = sem_val)
})
summary_stats <- t(summary_stats)
print(round(summary_stats, 3))
