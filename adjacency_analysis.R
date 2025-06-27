### ADD REFERENCE
### Behavioural Network Construction

# Packages and Data -------------------------------------------------------

# Packages
library(igraph)
library(dplyr)
library(tidyr)
library(stringr)

# Data
group1 <- read.csv("transition_unmated.csv")[, -1]
group2 <- read.csv("transition_mated.csv")[, -1]


# Helper Functions --------------------------------------------------------

get_transition_matrix <- function(df) {
  g <- graph_from_data_frame(df[, c("Behavior.0", "Behavior.1")], directed = TRUE)
  adj <- as_adjacency_matrix(g, sparse = FALSE)
  trans_prob <- round(adj / rowSums(adj), 4)
  trans_prob[is.nan(trans_prob)] <- 0
  return(trans_prob)
}

get_all_transition_matrices <- function(df) {
  df_list <- split(df, df$Mating.ID)
  trans_list <- lapply(df_list, get_transition_matrix)
  return(trans_list)
}

get_female_id <- function(mating_id) {
  str_extract(mating_id, "(?<=\\.)[A-Z0-9]+")
}


# Transition Matrices per Trial -------------------------------------------

trans_list1 <- get_all_transition_matrices(group1)
trans_list2 <- get_all_transition_matrices(group2)


# Wilcoxon Paired Tests ---------------------------------------------------

names(trans_list1) <- sapply(names(trans_list1), get_female_id)
names(trans_list2) <- sapply(names(trans_list2), get_female_id)

common_ids <- intersect(names(trans_list1), names(trans_list2))

all_behaviors <- unique(c(
  unlist(lapply(trans_list1, rownames)),
  unlist(lapply(trans_list2, rownames))
))

complete_matrix <- function(mat, behaviors) {
  full_mat <- matrix(0, nrow = length(behaviors), ncol = length(behaviors),
                     dimnames = list(behaviors, behaviors))
  
  row_inds <- intersect(rownames(mat), behaviors)
  col_inds <- intersect(colnames(mat), behaviors)
  full_mat[row_inds, col_inds] <- mat[row_inds, col_inds]
  
  return(full_mat)
}

paired_rows <- list()
row_index <- 1

for (id in common_ids) {
  mat1 <- complete_matrix(trans_list1[[id]], all_behaviors)
  mat2 <- complete_matrix(trans_list2[[id]], all_behaviors)
  
  for (from in all_behaviors) {
    for (to in all_behaviors) {
      paired_rows[[row_index]] <- data.frame(
        FemaleID = id,
        From = from,
        To = to,
        Unmated = mat1[from, to],
        Mated = mat2[from, to]
      )
      row_index <- row_index + 1
    }
  }
}

paired_data <- do.call(rbind, paired_rows)

test_results <- paired_data %>%
  group_by(From, To) %>%
  summarise(
    p_value = tryCatch(
      wilcox.test(Unmated, Mated, paired = TRUE, exact = FALSE)$p.value,
      error = function(e) NA
    ),
    mean_unmated = mean(Unmated),
    mean_mated = mean(Mated),
    .groups = "drop"
  )

test_results$p_adj <- p.adjust(test_results$p_value, method = "fdr")

sig_results <- filter(test_results, p_adj < 0.05)
