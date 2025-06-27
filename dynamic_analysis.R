### ADD REFERENCE
### Behavioural Network Construction


# Packages and Data -------------------------------------------------------

# Packages
library(dplyr)
library(readr)
library(tidyr)

# Data
mating_data <- read_csv("mating_data.csv")
mated <- read_csv("transition_mated.csv")
unmated <- read_csv("transition_unmated.csv")

mated$Group <- "mated"
unmated$Group <- "unmated"

df <- bind_rows(mated, unmated)

colnames(df)[2:3] <- c("From", "To")


# Total Sequenced Mating Duration -----------------------------------------

mating_data <- mating_data %>%
  mutate(total_mating_duration = 
           coalesce(first.courtship.duration, 0) + coalesce(first.mounting.duration, 0)) %>%
  select(ID, total_mating_duration)


# Total Transitions per Mating --------------------------------------------

transitions_per_experiment <- df %>%
  group_by(`Mating ID`, Group) %>%
  summarise(Total_Transitions = n(), .groups = "drop")


# Total Transitions / Total Duration --------------------------------------

df <- df %>%
  mutate(Mating_ID_core = sub(".*\\.", "", `Mating ID`))

transitions_per_experiment <- df %>%
  group_by(Mating_ID_core, Group) %>%
  summarise(Total_Transitions = n(), .groups = "drop")

transitions_per_experiment <- transitions_per_experiment %>%
  left_join(mating_data, by = c("Mating_ID_core" = "ID")) %>%
  mutate(Transitions_per_sec = Total_Transitions / total_mating_duration)


# In Degree per Mating ----------------------------------------------

in_degree <- df %>%
  group_by(Mating_ID_core, Group, Behavior = To) %>%
  summarise(In = n(), .groups = "drop")


# In Degree / Total Transitions and Total Duration ------------------------

in_degree <- in_degree %>%
  left_join(mating_data, by = c("Mating_ID_core" = "ID")) %>%
  left_join(transitions_per_experiment %>% select(Mating_ID_core, Total_Transitions), by = "Mating_ID_core") %>%
  mutate(
    In_per_transition = In / Total_Transitions,
    Transitions_per_sec = Total_Transitions / total_mating_duration
  )


# Wilcoxon Paired Tests for Total Transitions ---------------------------------------------------

transitions_per_experiment <- transitions_per_experiment %>%
  mutate(female = sub("x.*", "", Mating_ID_core))

total_transitions_wide <- transitions_per_experiment %>%
  select(female, Group, Transitions_per_sec) %>%
  pivot_wider(names_from = Group, values_from = Transitions_per_sec) %>%
  drop_na()

wilcox_total <- wilcox.test(total_transitions_wide$mated, total_transitions_wide$unmated, paired = TRUE) %>%
  broom::tidy() %>%
  mutate(test = "Total Transitions per sec")


# Wilcoxon Paired Tests for In Degree -------------------------------------------------------------------------

in_degree <- in_degree %>%
  mutate(female = sub("x.*", "", Mating_ID_core))

in_degree_wide <- in_degree %>%
  select(female, Behavior, Group, Transitions_per_sec, In_per_transition) %>%
  pivot_wider(names_from = Group, values_from = c(Transitions_per_sec, In_per_transition)) %>%
  drop_na()

run_wilcox_per_behavior <- function(df, var_prefix) {
  df %>%
    rowwise() %>%
    do({
      behavior = .$Behavior
      x = pull(., paste0(var_prefix, "_mated"))
      y = pull(., paste0(var_prefix, "_unmated"))
      if(length(x) == 0 || length(y) == 0) return(NULL)
      test_res = wilcox.test(x, y, paired = TRUE)
      tibble(
        Behavior = behavior,
        test = paste0(var_prefix),
        statistic = test_res$statistic,
        p.value = test_res$p.value
      )
    }) %>%
    ungroup()
}

wilcox_in_degree_Transitions <- in_degree_wide %>%
  group_by(Behavior) %>%
  summarise(
    test_res = list(
      wilcox.test(Transitions_per_sec_mated, Transitions_per_sec_unmated, paired = TRUE) %>% broom::tidy()
    )
  ) %>%
  unnest(cols = c(test_res)) %>%
  mutate(test = "In Degree Transitions_per_sec")

wilcox_in_degree_In_per_transition <- in_degree_wide %>%
  group_by(Behavior) %>%
  summarise(
    test_res = list(
      wilcox.test(In_per_transition_mated, In_per_transition_unmated, paired = TRUE) %>% broom::tidy()
    )
  ) %>%
  unnest(cols = c(test_res)) %>%
  mutate(test = "In Degree In_per_transition")


# Output ------------------------------------------------------------------

wilcox_results <- bind_rows(
  wilcox_total,
  wilcox_in_degree_Transitions %>% rename(statistic = statistic, p.value = p.value),
  wilcox_in_degree_In_per_transition %>% rename(statistic = statistic, p.value = p.value)
) %>%
  select(test, Behavior, statistic, p.value) %>%
  arrange(test, Behavior)

summary_total <- transitions_per_experiment %>%
  group_by(Group) %>%
  summarise(mean_Transitions_per_sec = mean(Transitions_per_sec, na.rm = TRUE))

summary_in_degree <- in_degree %>%
  group_by(Group, Behavior) %>%
  summarise(
    mean_Transitions_per_sec = mean(Transitions_per_sec, na.rm = TRUE),
    mean_In_per_transition = mean(In_per_transition, na.rm = TRUE),
    .groups = "drop"
  )

View(wilcox_results)
View(summary_total)
View(summary_in_degree)