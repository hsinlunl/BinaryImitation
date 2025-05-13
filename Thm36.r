library(igraph)

# Parameters
q_s <- 0.9
q_d <- 0.9
n <- 1000
simulations <- 100
max_iter <- 10000

# Function: check if current state is a global tie
is_global_tie <- function(g, opinions) {
  for (k in 1:vcount(g)) {
    neigh <- neighbors(g, k)
    if (length(neigh) == 0) next
    same <- sum(opinions[neigh] == opinions[k])
    diff <- length(neigh) - same
    if (same != diff) return(FALSE)
  }
  return(TRUE)
}

# Function: run one simulation and record fractions
run_model_c_tie_record <- function(g) {
  opinions <- sample(c(0, 1), n, replace = TRUE, prob = c(0.2, 0.8))  # 80% agents start with 1
  opinion_frac <- numeric(max_iter)

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

    opinion_frac[t] <- mean(opinions)

    if (is_global_tie(g, opinions)) {
      opinion_frac[(t + 1):max_iter] <- opinion_frac[t]
      break
    }
  }
  return(opinion_frac)
}

set.seed(123)
#g <- make_ring(n)
#repeat{
#  g <- sample_k_regular(no.of.nodes = n, k = 6)
#  if(is.connected(g)) break
#}
g <- make_full_bipartite_graph(n/2, n/2)
# Run simulations
fractions_matrix <- matrix(NA, nrow = max_iter, ncol = simulations)
for (i in 1:simulations) {
  fractions_matrix[, i] <- run_model_c_tie_record(g)
  if (i %% 100 == 0) cat("Completed", i, "simulations\n")
}

# Save to file
#saveRDS(fractions_matrix, "fractions_matrix_ModelC_tie.rds")

# Plot the trajectories
matplot(fractions_matrix, type = "l", lty = 1, col = rgb(1, 0, 0, 0.2),
        xlab = "Time step", ylab = "Fraction of opinion 1",
        main = "Model C: 1000 simulations (tie allowed)")
