### ADD REFERENCE
### Scoring analysis


# Helper Functions --------------------------------------------------------

read_behavior_data <- function(file_name, group, path) {
  read_csv(
    file.path(path, paste0(file_name, ".csv")),
    show_col_types = FALSE,
    name_repair = "minimal"
  ) %>%
    select(-1) %>%
    mutate(
      File = file_name,
      Group = group,
      Duration = as.numeric(Duration)
    ) %>%
    filter(!is.na(Duration))
}


# Packages and Data -------------------------------------------------------

# Packages 
library(dplyr)
library(readr)
library(purrr)
library(stringr)

# Data

files_g1 <- c(
  "Aggregated_G26xJ203", "Aggregated_G30xJ210", "Aggregated_H30xH71",
  "Aggregated_H45xH65", "Aggregated_H62xJ132", "Aggregated_J101xJ204",
  "Aggregated_J102xJ206", "Aggregated_J110xH67", "Aggregated_J124xG42",
  "Aggregated_J144xG39", "Aggregated_J183xH26", "Aggregated_J189xJ112",
  "Aggregated_J190xJ42", "Aggregated_J191xJ68", "Aggregated_J201xG43"
)

files_g2 <- c(
  "Aggregated_G30xH52", "Aggregated_H30xG38", "Aggregated_H62xG33",
  "Aggregated_J101xH76", "Aggregated_J102xJ161", "Aggregated_J110xJ140",
  "Aggregated_J124xG32", "Aggregated_J144xG38", "Aggregated_J183xG35",
  "Aggregated_J190xJ212"
)

path_g1 <- "data_scoring/unmated"
path_g2 <- "data_scoring/mated"

data_g1 <- map_dfr(files_g1, read_behavior_data, group = "unmated", path = path_g1)
data_g2 <- map_dfr(files_g2, read_behavior_data, group = "mated", path = path_g2)

full_data <- bind_rows(data_g1, data_g2)


# Get Time Frequency / Behaviour / Individual -----------------------------

behaviour_summary <- full_data %>%
  group_by(Group, File, Behaviour) %>%
  summarise(TotalBehaviourTime = sum(Duration), .groups = "drop") %>%
  left_join(
    full_data %>%
      group_by(Group, File) %>%
      summarise(TotalTime = sum(Duration), .groups = "drop"),
    by = c("Group", "File")
  ) %>%
  mutate(TimeFrequency = TotalBehaviourTime / TotalTime)


# Summarise Behaviours ----------------------------------------------------

behaviour_stats <- behaviour_summary %>%
  group_by(Group, Behaviour) %>%
  summarise(
    MeanTimeFreq = mean(TimeFrequency),
    SETimeFreq = sd(TimeFrequency) / sqrt(n()),
    .groups = "drop"
  )


# Paired Wilcoxon Tests ----------------------------------------------------

# Extract unique individual ID (first part after Aggregated_)
behaviour_summary <- behaviour_summary %>%
  mutate(ID = str_extract(File, "(?<=Aggregated_)[^x]+"))

# Spread into wide format to enable paired test
behaviour_wide <- behaviour_summary %>%
  select(ID, Group, Behaviour, TimeFrequency) %>%
  pivot_wider(names_from = Group, values_from = TimeFrequency)

# Do paired Wilcoxon test per behaviour
wilcoxon_results <- behaviour_wide %>%
  group_by(Behaviour) %>%
  summarise(
    p_value = if (sum(!is.na(unmated) & !is.na(mated)) >= 3) { # ensure enough data
      wilcox.test(unmated, mated, paired = TRUE, exact = FALSE)$p.value
    } else { NA },
    .groups = "drop"
  )


# Output ------------------------------------------------------------------

final_results <- behaviour_stats %>%
  pivot_wider(names_from = Group, values_from = c(MeanTimeFreq, SETimeFreq)) %>%
  left_join(wilcoxon_results, by = "Behaviour")


# Additionnal - Behaviour Count -------------------------------------------

behaviour_counts <- full_data %>%
  group_by(Group, Behaviour, File) %>%  # Group by file to count distinct trials
  summarise(TotalTime = sum(Duration), .groups = "drop") %>%
  group_by(Group, Behaviour) %>%
  summarise(n_trials = n(), .groups = "drop")

