### ADD REFERENCE
### Influence of Female Mating Status on Male Courtship Structure
### Construction of Courtship Behavioural Network (iGraph version 2.1.4)


# Packages and Data -------------------------------------------------------

# Packages
library(data.table)
library(tidyverse)
library(igraph)

# Data
group1 <- read.csv("transition_unmated.csv")[, -c(1, 4)]
group2 <- read.csv("transition_mated.csv")[, -c(1, 4)]


# Observed Adjacency Matrix -----------------------------------------------

# Create directed graphs from the data frames
graph1 <- graph_from_data_frame(group1, directed = T)
graph2 <- graph_from_data_frame(group2, directed = T)

# Convert graphs to adjacency matrices
adjacency.matrix1 <- as_adjacency_matrix(graph1, sparse = F)
adjacency.matrix2 <- as_adjacency_matrix(graph2, sparse = F)


# Permutation and Random Matrices --------------------------------------------------------

m <- 50000 # Number of permutations

# For group1: create list of adjacency matrices after permuting second column m times
results1 <- as.list(NA)
for (i in 1:m) {
  perm <- group1
  perm[, 2] <- perm[sample(1:nrow(perm), replace = FALSE), 2]
  perm.graph <- graph_from_data_frame(perm, directed = T)
  perm.adjacency.matrix <- as_adjacency_matrix(perm.graph, sparse = F)
  results1[[i]] <- perm.adjacency.matrix
}

# Same permutation procedure for group2
results2 <- as.list(NA)
for (i in 1:m) {
  perm <- group2
  perm[, 2] <- perm[sample(1:nrow(perm), replace = FALSE), 2]
  perm.graph <- graph_from_data_frame(perm, directed = T)
  perm.adjacency.matrix <- as_adjacency_matrix(perm.graph, sparse = F)
  results2[[i]] <- perm.adjacency.matrix
}

# Flatten adjacency matrices to vectors and combine into matrices
listVec1 <- lapply(results1, c, recursive = TRUE)
x1 <- do.call(cbind, listVec1)
listVec2 <- lapply(results2, c, recursive = TRUE)
x2 <- do.call(cbind, listVec1)


# 5% and 95% Quantiles -------------------------------------------------------------------------

# Calculate 5% and 95% quantiles for each edge (row) in the permutation results for group1
quantiles1 <- matrix(NA, nrow = nrow(x1), ncol = 2)
for (i in 1:nrow(x1)) {
  q <- quantile(x1[i, ], probs = c(0.05, 0.95))
  quantiles1[i, ] <- q
}

# Same quantile calculations for group2
quantiles2 <- matrix(NA, nrow = nrow(x2), ncol = 2)
for (i in 1:nrow(x2)) {
  q <- quantile(x2[i, ], probs = c(0.05, 0.9))
  quantiles2[i, ] <- q
}

# Convert quantile vectors back into adjacency matrix shape
low1 <- matrix(quantiles1[, 1], nrow = 14, byrow = F)
high1 <- matrix(quantiles1[, 2], nrow = 14, byrow = F)
low2 <- matrix(quantiles2[, 1], nrow = 14, byrow = F)
high2 <- matrix(quantiles2[, 2], nrow = 14, byrow = F)

# Assign proper row and column names to quantile matrices matching observed matrices
colnames(low1) <- colnames(adjacency.matrix1)
rownames(low1) <- colnames(adjacency.matrix1)
colnames(high1) <- colnames(adjacency.matrix1)
rownames(high1) <- colnames(adjacency.matrix1)
# -
colnames(low2) <- colnames(adjacency.matrix2)
rownames(low2) <- colnames(adjacency.matrix2)
colnames(high2) <- colnames(adjacency.matrix2)
rownames(high2) <- colnames(adjacency.matrix2)


# Observed Transition Probabilities ---------------------------------------

# Calculate observed transition probabilities by normalizing rows
trans.prob1 <- round(adjacency.matrix1 / rowSums(adjacency.matrix1), 2)
trans.prob1[is.nan(trans.prob1)] <- 0
# -
trans.prob2 <- round(adjacency.matrix2 / rowSums(adjacency.matrix2), 2)
trans.prob2[is.nan(trans.prob2)] <- 0


# Simplify to Significant Probabilities -----------------------------------

# Keep transition probabilities where observed adjacency is greater than upper 95% quantile; else zero
keep1 <- ifelse(adjacency.matrix1 >= high1, trans.prob1, 0)
keep2 <- ifelse(adjacency.matrix2 >= high2, trans.prob2, 0)


# Create Network ----------------------------------------------------------

# Create weighted directed networks from thresholded transition probabilities
network1 <- graph_from_adjacency_matrix(keep1, weighted = TRUE, diag = TRUE)
network2 <- graph_from_adjacency_matrix(keep2, weighted = TRUE, diag = TRUE)

# Categorize edge weights into 5 bins for plotting
plot1 <- tkplot(network1)
plot2 <- tkplot(network2)

