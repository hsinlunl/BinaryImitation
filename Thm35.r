library(igraph)

# Parameters
q_s <- 0.1
q_d <- 0
n <- 1000
simulations <- 1000
max_iter <- 10000  # To keep plots readable

# Create a connected graph
set.seed(42)
repeat {
  g <- sample_gnp(n, p = 0.1)
  if (is.connected(g)) break
}

# Function to run one simulation and record the opinion 1 fraction
simulate_one_model_c <- function(g) {
  opinions <- sample(c(0, 1), vcount(g), replace = TRUE)
  fractions <- numeric(max_iter)
  
  for (t in 1:max_iter) {
    fractions[t] <- mean(opinions)
    
    k <- sample(1:vcount(g), 1)
    neighbors_k <- neighbors(g, k)
    if (length(neighbors_k) == 0) next
    
    same <- sum(opinions[neighbors_k] == opinions[k])
    diff <- length(neighbors_k) - same
    
    if (diff > same) {
      if (runif(1) < q_d) opinions[k] <- 1 - opinions[k]
    } else if (same > diff) {
      if (runif(1) < q_s) opinions[k] <- 1 - opinions[k]
    }
    
    if (all(opinions == 0) || all(opinions == 1)) {
      fractions[(t+1):max_iter] <- mean(opinions)
      break
    }
  }
  
  return(fractions)
}

# Run 1000 simulations
fractions_matrix <- matrix(NA, nrow = max_iter, ncol = simulations)
for (i in 1:simulations) {
  fractions_matrix[, i] <- simulate_one_model_c(g)
  if (i %% 100 == 0) cat("Completed", i, "simulations\n")
}

# Plot all simulations
matplot(fractions_matrix, type = "l", lty = 1, col = rgb(0, 0, 1, 0.1),
        xlab = "Time step", ylab = "Fraction of opinion 1",
        main = "Model C: 1000 Simulations of Opinion Dynamics")

saveRDS(fractions_matrix, file = "fractions_matrix_ModelC.rds")
fractions_matrix <- readRDS("fractions_matrix_ModelC.rds")
