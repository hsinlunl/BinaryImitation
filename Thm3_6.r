library(igraph)

# Parameters
q_s <- 0.3
q_d <- 0.3
n <- 1000
simulations <- 1000
max_iter <- 10000

# Function: check if current state is a global tie
is_global_tie <- function(g, opinions) {
  for (k in 1:vcount(g)) {
    neigh <- neighbors(g, k)
    if (length(neigh) == 0) next  # isolated node
    same <- sum(opinions[neigh] == opinions[k])
    diff <- length(neigh) - same
    if (same != diff) return(FALSE)
  }
  return(TRUE)
}

# Function: run one simulation
run_model_c_tie <- function(g) {
  opinions <- sample(c(0, 1), vcount(g), replace = TRUE)
  
  for (t in 1:max_iter) {
    k <- sample(1:vcount(g), 1)
    neigh <- neighbors(g, k)
    if (length(neigh) == 0) next
    same <- sum(opinions[neigh] == opinions[k])
    diff <- length(neigh) - same
    
    if (diff > same) {
      if (runif(1) < q_d) opinions[k] <- 1 - opinions[k]
    } else if (same > diff) {
      if (runif(1) < q_s) opinions[k] <- 1 - opinions[k]
    }
    
    if (is_global_tie(g, opinions)) return("tie")
  }
  return("diverge")
}

# Create graph with even degree (to allow for tie state)
g <- sample_k_regular(no.of.nodes = n, k = 6)  # Regular graph of even degree

# Run multiple simulations
results <- replicate(simulations, run_model_c_tie(g))
table(results)
