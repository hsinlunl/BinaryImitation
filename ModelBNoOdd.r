library(igraph)
set.seed(123)
n <- 1000 # number of vertices
n1 <- n/2  # size of partition 1
n2 <- n/2  # size of partition 2
m <- 10000  # number of simulations

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

    # Apply Model B imitation rule
    if (opinions[i] == opinions[j]) {
      neighbors_union <- union(neighbors(g, i), neighbors(g, j))
      opinions[neighbors_union] <- opinions[i]
    } else {
      ni <- setdiff(neighbors(g, i), neighbors(g, j))
      nj <- setdiff(neighbors(g, j), neighbors(g, i))
      opinions[ni] <- opinions[j]
      opinions[nj] <- opinions[i]
    }

    # Check convergence
    result <- has_converged(g, opinions)
    if (result$consensus || result$alternating) break
  }

  return(result)
}

# Generate one connected bipartite graph without odd cycles
repeat {
  g <- sample_bipartite(n1, n2, type = "gnp", p = 0.1)
  if (is.connected(g)) break
}

# Initialize counters
consensus_cumulative <- numeric(m)
alternating_cumulative <- numeric(m)
consensus_count <- 0
alternating_count <- 0

# Run simulations and track cumulative proportions
for (i in 1:m) {
  outcome <- run_model_once(g)
  if (outcome$consensus) consensus_count <- consensus_count + 1
  if (outcome$alternating) alternating_count <- alternating_count + 1
  consensus_cumulative[i] <- consensus_count / i
  alternating_cumulative[i] <- alternating_count / i
}

# Create a data frame for plotting
df_time <- data.frame(
  Simulation = 1:m,
  Consensus = consensus_cumulative,
  Alternating = alternating_cumulative
)

# Load ggplot2 for plotting
library(ggplot2)

# Plot
ggplot(df_time, aes(x = Simulation)) +
  geom_line(aes(y = Consensus, color = "Consensus"), size = 1) +
  geom_line(aes(y = Alternating, color = "Alternating"), size = 1) +
  scale_color_manual(values = c("Consensus" = "steelblue", "Alternating" = "firebrick")) +
  labs(title = "Cumulative Proportion over Simulations (Model B without an odd cycle)",
       x = "Simulation Number",
       y = "Cumulative Proportion",
       color = "Final State") +
  theme_minimal() +
  theme(text = element_text(size = 14))
