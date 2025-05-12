library(igraph)
set.seed(123)
num_simulations <- 1000
max_steps <- 110
fractions_matrix <- matrix(NA, nrow = max_steps, ncol = num_simulations)

n <- 1000
p <- 0.1
repeat {
  g <- erdos.renyi.game(n, p, directed = FALSE)
  if (is.connected(g) && !is_bipartite(g)) break
}

for (sim in 1:num_simulations) {
  opinions <- sample(c(0, 1), n, replace = TRUE)
  opinion_frac <- numeric(max_steps)
  
  for (t in 1:max_steps) {
    edge <- sample(E(g), 1)
    pair <- ends(g, edge)
    i <- as.integer(pair[1])
    j <- as.integer(pair[2])
    
    if (opinions[i] == opinions[j]) {
      neighbors <- union(neighbors(g, i), neighbors(g, j))
      opinions[neighbors] <- opinions[i]
    } 
    
    opinion_frac[t] <- mean(opinions)
    
    if (all(opinions == 0) || all(opinions == 1)) {
      # Fill the rest with the final value
      opinion_frac[(t+1):max_steps] <- opinion_frac[t]
      break
    }
  }
  
  # Store the full-length opinion fraction vector
  fractions_matrix[, sim] <- opinion_frac
}

# Plot the trajectories
matplot(fractions_matrix, type = "l", lty = 1, col = rgb(0, 0, 1, 0.2),
        xlab = "Time step", ylab = "Fraction of opinion 1", 
        main = "Model A with an odd cycle: 1000 simulations on the same graph")

saveRDS(fractions_matrix, file = "fractions_matrixModelAOdd.rds")
fractions_matrix <- readRDS("fractions_matrixModelAOdd.rds")
