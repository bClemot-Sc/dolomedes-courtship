### ADD REFERENCE
### Global analysis


# Packages and Data -------------------------------------------------------

# Import packages
library(tidyverse)
library(nortest)
library(ggplot2)
library(tools)
library(stringr)
set.seed(42)

# Import data
data <- read.csv("mating_data.csv")
unmated.df <- data %>% filter(female.mated.before==FALSE)
mated.df <- data %>% filter(female.mated.before==TRUE)


# Normality Tests --------------------------------------------------------

variables <- c("latency.to.court", "first.courtship.duration",
               "latency.to.mount", "first.mounting.duration")

# Q-Q plots
par(mfrow = c(2, 4), oma = c(0, 0, 2, 0))
for (var in variables) {
  qqnorm(unmated.df[[var]], main = paste("Unmated -", gsub("\\.", " ", var)))
  qqline(unmated.df[[var]], col = "red")
  # -
  qqnorm(mated.df[[var]], main = paste("Mated -", gsub("\\.", " ", var)))
  qqline(mated.df[[var]], col = "blue")
}
mtext("QQ Plots - Unmated vs. Mated Groups", outer = TRUE, cex = 1.5)

# Histograms
par(mfrow = c(4, 2), oma = c(0, 0, 2, 0), mar = c(4, 4, 3, 1)) 
for (var in variables) {
  hist(unmated.df[[var]], probability = TRUE, 
       main = paste("Unmated -", gsub("\\.", " ", var)),
       col = "lightblue", xlab = gsub("\\.", " ", var), breaks = 10)
  lines(density(unmated.df[[var]], na.rm = TRUE), col = "red", lwd = 2)
  # -
  hist(mated.df[[var]], probability = TRUE, 
       main = paste("Mated -", gsub("\\.", " ", var)),
       col = "lightgreen", xlab = gsub("\\.", " ", var), breaks = 10)
  lines(density(mated.df[[var]], na.rm = TRUE), col = "blue", lwd = 2)
}
mtext("Histograms with Density - Unmated vs. Mated Groups", outer = TRUE, cex = 1.5)

# Statistical Tests
data %>%
  group_by(female.mated.before) %>%
  summarise(across(all_of(variables), list), .groups = "drop") %>%
  pivot_longer(-female.mated.before, names_to = "Variable", values_to = "Values") %>%
  mutate(
    Shapiro = lapply(Values, shapiro.test),
    AD = lapply(Values, ad.test),
    Shapiro_W = sapply(Shapiro, function(x) round(x$statistic, 3)),
    Shapiro_p = sapply(Shapiro, function(x) round(x$p.value, 4)),
    AD_A = sapply(AD, function(x) round(x$statistic, 3)),
    AD_p = sapply(AD, function(x) round(x$p.value, 4)),
    Group = ifelse(female.mated.before, "mated", "unmated"),
    Variable = gsub("\\.", " ", Variable)
  ) %>%
  select(Group, Variable, Shapiro_W, Shapiro_p, AD_A, AD_p) %>%
  print()


# Comparison Tests --------------------------------------------------------

# Wilcoxon Signed-Rank Test (paired)
results <- data.frame(Variable = character(), W = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
for (var in variables) {
  wide_data <- data %>%
    select(female, female.mated.before, all_of(var)) %>%
    pivot_wider(names_from = female.mated.before, values_from = all_of(var)) %>%
    rename(unmated = `FALSE`, mated = `TRUE`)
  test <- wilcox.test(wide_data$unmated, wide_data$mated, paired = TRUE)
  results <- rbind(results, data.frame(Variable = var, W = test$statistic, p_value = test$p.value))
}
print(results)

# Wilcoxon Rank-Sum Test (non-paired)
results_nonpaired <- lapply(variables, function(var) {
  test_data <- data %>% filter(!is.na(.data[[var]]))
  test <- wilcox.test(as.formula(paste(var, "~ female.mated.before")), data = test_data, paired = FALSE)
  data.frame(Variable = var, W = test$statistic, p_value = test$p.value)
}) %>% bind_rows()
print(results_nonpaired)

# Wilcoxon Rank-Sum Test (non-paired; with removed incomplete pairs)
results_nonpaired_complete <- lapply(variables, function(var) {
  complete_females <- data %>%
    group_by(female) %>%
    filter(!any(is.na(.data[[var]]))) %>%
    ungroup()
  test <- wilcox.test(as.formula(paste(var, "~ female.mated.before")), data = complete_females, paired = FALSE)
  data.frame(Variable = var, W = test$statistic, p_value = test$p.value)
}) %>% bind_rows()
print(results_nonpaired_complete)


# Graphical Representation (paired-filtered) ------------------------------------------------

# Prepare long-form full data
all_data <- data %>%
  select(female, female.mated.before, all_of(variables)) %>%
  pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = gsub("\\.", " ", Variable) |> str_to_title())

# Get cleaned data (paired only)
clean_data <- all_data %>%
  group_by(female, Variable) %>%
  mutate(paired = if (any(is.na(Value))) NA_real_ else Value) %>%
  ungroup() %>%
  mutate(CleanValue = paired) %>%
  select(-paired)

# Add flags
plot_data <- clean_data %>%
  mutate(
    Group = ifelse(female.mated.before, "Mated", "Unmated"),
    Variable = factor(Variable, levels = c(
      "Latency To Court", "First Courtship Duration", "Latency To Mount", "First Mounting Duration"
    )),
    Group = factor(Group, levels = c("Unmated", "Mated")),
    Missing = is.na(CleanValue) & !is.na(Value)  # flagged in red
  )

# Colour palette
cb_palette <- c("Unmated" = "#E69F00", "Mated" = "#56B4E9")

# Plot
ggplot(plot_data, aes(x = Group, y = CleanValue, fill = Group)) +
  geom_violin(alpha = 0.4, color = NA, show.legend = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, show.legend = FALSE) +
  
  geom_jitter(
    data = subset(plot_data, !Missing),
    width = 0.05, size = 1.5, alpha = 0.7,
    shape = 21, color = "black", show.legend = FALSE
  ) +
  
  geom_jitter(
    data = subset(plot_data, Missing),
    aes(y = Value),
    width = 0.08, size = 1.5, alpha = 0.7,
    shape = 21, fill = "red", color = "black", show.legend = FALSE
  ) +
  
  scale_fill_manual(values = cb_palette) +
  scale_y_continuous(limits = c(-100, NA), expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  labs(
    title = "Comparison of Behavioural Variables (s) between Groups filtered by Pairs",
    x = "",
    y = "Value",
    caption = "Points represent individual measurements; Red points were removed for pair analysis\nViolins show distribution; Boxes show median and IQR"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(size = 13),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.caption = element_text(size = 10, face = "italic"),
    legend.position = "none"
  )


# Graphical Representation (non-paired filtered) --------------------------

# Prepare long data
plot_data2 <- data %>%
  select(female, female.mated.before, all_of(variables)) %>%
  pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Value") %>%
  mutate(
    Group = factor(ifelse(female.mated.before, "Mated", "Unmated"),
                   levels = c("Unmated", "Mated")),
    Variable = recode(Variable,
                      "latency.to.court" = "Latency To Court",
                      "first.courtship.duration" = "First Courtship Duration",
                      "latency.to.mount" = "Latency To Mount",
                      "first.mounting.duration" = "First Mounting Duration"
    )
  )

# Add flags
plot_data2$Variable <- factor(plot_data2$Variable, levels = c(
  "Latency To Court",
  "First Courtship Duration",
  "Latency To Mount",
  "First Mounting Duration"
))

# Colour palette
cb_palette <- c("Unmated" = "#D81B60", "Mated" = "#56B4E9")

# Plot
ggplot(plot_data2, aes(x = Group, y = Value, fill = Group)) +
  geom_violin(alpha = 0.4, color = NA, show.legend = FALSE) +
  geom_boxplot(width = 0.1, outlier.shape = NA, show.legend = FALSE) +
  
  geom_jitter(width = 0.08, size = 1.5, alpha = 0.7, shape = 21, color = "black", show.legend = FALSE) +
  
  scale_fill_manual(values = cb_palette) +
  scale_y_continuous(limits = c(-100, NA), expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  labs(
    title = "Comparison of Behavioural Variables (s) between Groups non-filtered by Paires",
    x = "",
    y = "Value",
    caption = "Points represent individual measurements; violins show distribution; boxes show median and IQR."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(size = 13),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.caption = element_text(size = 10, face = "italic")
  )
