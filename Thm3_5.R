library(igraph)

# ---- Model C Simulation ----

# Parameters
q_s <- 0.1   # Flip when majority agrees
q_d <- 0     # No flip when majority disagrees
n <- 1000     # Number of agents (nodes)
simulations <- 1000  # Number of trials
max_iter <- 10000    # Max steps per simulation

# Function to run one simulation
run_model_c_once <- function(g) {
  opinions <- sample(c(0, 1), vcount(g), replace = TRUE)
  
  for (t in 1:max_iter) {
    k <- sample(1:vcount(g), 1)
    neighbors_k <- neighbors(g, k)
    if (length(neighbors_k) == 0) next
    
    same <- sum(opinions[neighbors_k] == opinions[k])
    diff <- length(neighbors_k) - same
    
    if (diff > same) {
      # Majority disagrees: flip with q_d = 0
      if (runif(1) < q_d) opinions[k] <- 1 - opinions[k]
    } else if (same > diff) {
      # Majority agrees: flip with q_s
      if (runif(1) < q_s) opinions[k] <- 1 - opinions[k]
    }
    # if same == diff: do nothing
    
    if (all(opinions == 0) || all(opinions == 1)) return(TRUE)  # Consensus reached
  }
  return(FALSE)  # No consensus within max_iter
}

# ---- Run Many Simulations ----

set.seed(42)
repeat{
g <- sample_gnp(n, p = 0.1)  # Erdos-Renyi random graph (connected)
if (is.connected(g)) break
}

consensus_count <- 0
for (i in 1:simulations) {
  if (run_model_c_once(g)) consensus_count <- consensus_count + 1
  if (i %% 100 == 0) cat("Completed", i, "simulations\n")
}

cat("Proportion of simulations reaching consensus:", consensus_count / simulations, "\n")
