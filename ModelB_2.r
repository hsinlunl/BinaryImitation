# Set sizes of the two partitions
n1 <- 500  # size of partition 1
n2 <- 500  # size of partition 2
max_steps <- 30000        # maximum simulation steps
n<-n1+n2

set.seed(111)

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

repeat {
  # Generate random bipartite graph
  g <- sample_bipartite(n1, n2, type = "gnp", p = 0.01)

  # Check if the graph is connected
  if (is.connected(g)) break
}

# Initialize binary opinions (0 or 1)
opinions <- sample(c(0, 1), n, replace = TRUE)

# Track fraction of opinion 1 over time
opinion_frac <- numeric(max_steps)

# Run Model B dynamics
for (t in 1:max_steps) {
  # Pick a random edge
  edge <- sample(E(g), 1)
  pair <- ends(g, edge)
  i <- as.integer(pair[1])
  j <- as.integer(pair[2])
  
  if (opinions[i] == opinions[j]) {
    # All neighbors of i and j adopt common opinion
    neighbors <- union(neighbors(g, i), neighbors(g, j))
    opinions[neighbors] <- opinions[i]
  } else {
    # Neighbors of j not in i adopt i's opinion
    nj_not_ni <- setdiff(neighbors(g, j), neighbors(g, i))
    opinions[nj_not_ni] <- opinions[i]
    
    # Neighbors of i not in j adopt j's opinion
    ni_not_nj <- setdiff(neighbors(g, i), neighbors(g, j))
    opinions[ni_not_nj] <- opinions[j]
  }
  
  # Record fraction of opinion 1
  opinion_frac[t] <- mean(opinions)
 
  # Stop if consensus is reached
  if (all(opinions == 0) || all(opinions == 1) || is_alternating_state(g,opinions)) {
    opinion_frac <- opinion_frac[1:t]
    break
  }
}

# Plot opinion dynamics
plot(opinion_frac, type = "l", col = "blue", lwd = 2,
     xlab = "Time step", ylab = "Fraction of opinion 1",
     main = "Consensus dynamics in Model B")
abline(h = c(0, 1), col = "gray", lty = 2)

has_odd_cycle <- !is_bipartite(g)

if (has_odd_cycle) {
  cat("The graph contains an odd cycle.\n")
} else {
  cat("The graph does NOT contain an odd cycle.\n")
}