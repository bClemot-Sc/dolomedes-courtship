### ADD REFERENCE
### Behavioural Network Construction


# Packages and Data -------------------------------------------------------

# Packages
library(data.table)
library(tidyverse)
library(igraph)

# Data
group1 <- read.csv("transition_unmated.csv")[, -c(1, 4)]
group2 <- read.csv("transition_mated.csv")[, -c(1, 4)]


# Observed Adjacency Matrix -----------------------------------------------

graph1 <- graph_from_data_frame(group1, directed = T)
graph2 <- graph_from_data_frame(group2, directed = T)

adjacency.matrix1 <- as_adjacency_matrix(graph1, sparse = F)
adjacency.matrix2 <- as_adjacency_matrix(graph2, sparse = F)


# Permutation and Random Matrices --------------------------------------------------------

m <- 100

# Randomly shuffle the second column m times and keep each new adjacency matrix in a list
results1 <- as.list(NA)
for (i in 1:m) {
  perm <- group1
  perm[, 2] <- perm[sample(1:nrow(perm), replace = FALSE), 2]
  perm.graph <- graph_from_data_frame(perm, directed = T)
  perm.adjacency.matrix <- as_adjacency_matrix(perm.graph, sparse = F)
  results1[[i]] <- perm.adjacency.matrix
}

results2 <- as.list(NA)
for (i in 1:m) {
  perm <- group2
  perm[, 2] <- perm[sample(1:nrow(perm), replace = FALSE), 2]
  perm.graph <- graph_from_data_frame(perm, directed = T)
  perm.adjacency.matrix <- as_adjacency_matrix(perm.graph, sparse = F)
  results2[[i]] <- perm.adjacency.matrix
}

# Reorganise list structure
listVec1 <- lapply(results1, c, recursive = TRUE)
x1 <- do.call(cbind, listVec1)
listVec2 <- lapply(results2, c, recursive = TRUE)
x2 <- do.call(cbind, listVec1)


# 5% and 95% Quantiles -------------------------------------------------------------------------

quantiles1 <- matrix(NA, nrow = nrow(x1), ncol = 2)
for (i in 1:nrow(x1)) {
  q <- quantile(x1[i, ], probs = c(0.05, 0.95))
  quantiles1[i, ] <- q
}

quantiles2 <- matrix(NA, nrow = nrow(x2), ncol = 2)
for (i in 1:nrow(x2)) {
  q <- quantile(x2[i, ], probs = c(0.05, 0.95))
  quantiles2[i, ] <- q
}

# Save matrices for each of the low and high quantile data
low1 <- matrix(quantiles1[, 1], nrow = 14, byrow = F)
high1 <- matrix(quantiles1[, 2], nrow = 14, byrow = F)
low2 <- matrix(quantiles2[, 1], nrow = 14, byrow = F)
high2 <- matrix(quantiles2[, 2], nrow = 14, byrow = F)

# Rename the low and high quantile datasets to match the observed adjacency matrix
colnames(low1) <- colnames(adjacency.matrix1)
rownames(low1) <- colnames(adjacency.matrix1)
colnames(high1) <- colnames(adjacency.matrix1)
rownames(high1) <- colnames(adjacency.matrix1)

colnames(low2) <- colnames(adjacency.matrix2)
rownames(low2) <- colnames(adjacency.matrix2)
colnames(high2) <- colnames(adjacency.matrix2)
rownames(high2) <- colnames(adjacency.matrix2)


# Observed Transition Probabilities ---------------------------------------

trans.prob1 <- round(adjacency.matrix1 / rowSums(adjacency.matrix1), 2)
trans.prob1[is.nan(trans.prob1)] <- 0

trans.prob2 <- round(adjacency.matrix2 / rowSums(adjacency.matrix2), 2)
trans.prob2[is.nan(trans.prob2)] <- 0


# Simplify to Significant Probabilities -----------------------------------

keep1 <- ifelse(adjacency.matrix1 >= high1, trans.prob1, 0)
keep2 <- ifelse(adjacency.matrix2 >= high2, trans.prob2, 0)


# Create Network ----------------------------------------------------------

network1 <- graph_from_adjacency_matrix(keep1, weighted = TRUE, diag = TRUE)
network2 <- graph_from_adjacency_matrix(keep2, weighted = TRUE, diag = TRUE)

edge_weights1 <- as.numeric(cut(E(network1)$weight, c(0, .1, .25, .5, .75, 1), labels =
                                  c(1:5)))
edge_weights2 <- as.numeric(cut(E(network2)$weight, c(0, .1, .25, .5, .75, 1), labels =
                                  c(1:5)))
plot1 <- tkplot(network1)
plot2 <- tkplot(network2)

coords1 <- tk_coords(plot1)
coords2 <- tk_coords(plot2)

plot.igraph(
  network1,
  vertex.label = V(network1)$name,
  vertex.label.cex = 1.25,
  layout = coords1,,
  edge.width = edge_weights1 / 4,
  edge.arrow.size = edge_weights1 / 4
)
plot.igraph(
  network2,
  vertex.label = V(network2)$name,
  vertex.label.cex = 1.25,
  layout = coords2,,
  edge.width = edge_weights2 / 4,
  edge.arrow.size = edge_weights2 / 4
)

