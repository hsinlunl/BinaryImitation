library(igraph)
n <- 1000
n1 <- n/2  # size of partition 1
n2 <- n/2  # size of partition 2

# ----- Check for consensus or alternating state -----
is_alternating_state <- function(g, opinions) {
  edges <- as_edgelist(g)
  for (e in 1:nrow(edges)) {
    v1 <- as.integer(edges[e, 1])
    v2 <- as.integer(edges[e, 2])
    if (opinions[v1] == opinions[v2]) {
      return(FALSE)
    }
  }
  return(TRUE)
}

has_converged <- function(g, opinions) {
  consensus <- all(opinions == 0) || all(opinions == 1)
  alternating <- is_alternating_state(g, opinions)
  list(consensus = consensus, alternating = alternating)
}

run_model_once <- function(g) {
  # Initialize opinions randomly
  n <- vcount(g)
  opinions <- sample(c(0, 1), n, replace = TRUE)

  repeat {
    # Randomly select an edge
    edge <- sample(1:ecount(g), 1)
    ends <- ends(g, edge)
    i <- as.integer(ends[1])
    j <- as.integer(ends[2])

    # Apply Model A imitation rule
    if (opinions[i] == opinions[j]) {
      neighbors_union <- union(neighbors(g, i), neighbors(g, j))
      opinions[neighbors_union] <- opinions[i]
    } 
    # Check convergence
    result <- has_converged(g, opinions)
    if (result$consensus || result$alternating) break
  }

  return(result)
}

# ----- Monte Carlo Simulation -----
simulate_model <- function(g, m = 1000) {
  consensus_count <- 0
  alternating_count <- 0
  for (i in 1:m) {
    outcome <- run_model_once(g)
    if (outcome$consensus) consensus_count <- consensus_count + 1
    if (outcome$alternating) alternating_count <- alternating_count + 1
  }
  return(list(
    consensus_proportion = consensus_count / m,
    alternating_proportion = alternating_count / m
  ))
}

# ----- Example -----
set.seed(123)
start_time <- Sys.time()
repeat {
  # Generate random bipartite graph
  g <- sample_bipartite(n1, n2, type = "gnp", p = 0.1)

  # Check if the graph is connected
  if (is.connected(g)) break
}
result <- simulate_model(g, m = 1000)
print(result)
end_time <- Sys.time()
elapsed <- end_time - start_time

# Install ggplot2 if not already installed
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Prepare data frame for plotting
# Example in R
results <- data.frame(
  Model = c("A (odd cycle)", "A (no odd cycle)", "B (odd cycle)"),
  ProportionConsensus = c(1, 1, 1)
)

library(ggplot2)
ggplot(results, aes(x = Model, y = ProportionConsensus)) +
  geom_bar(stat = "identity", fill = "steelblue",width=0.6) +
  ylim(0, 1) +
  labs(title = "Proportion of Consensus Achieved",
       y = "Proportion", x = "") +
  theme_minimal()


