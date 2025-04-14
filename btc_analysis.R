library(igraph)

# Import data
btc_alpha <- read.csv("D:/12week2024/3rd/Fall2024/Intro. to social/Intro_to_social_project/soc-sign-bitcoinalpha.csv",header=TRUE)

# construct the graph
g <- graph_from_data_frame(btc_alpha, directed=TRUE)
head(V(g))
head(E(g))
summary(g)
is_directed(g)

# add edges' weights 
E(g)$weight <- btc_alpha$Rating
is_weighted(g)
## check the weights histograms
hist(E(g)$weight, breaks=20, col="lightblue", main="Distribution of Edge Weights")

###########################################################################################
# Node Degree

V(g)$indegree <- degree(g, mode="in")
V(g)$outdegree <- degree(g, mode="out")

## Degree Distribution
hist(degree(g), col="lightgreen", main="Degree Distribution")

# Calculate the average degree
num_nodes <- vcount(g)
num_edges <- ecount(g)
average_degree <- num_edges / num_nodes
cat("Average Degree:", average_degree, "\n")

##############################################################################################
#Representation:

# 1) Graph Representation :

### Adjacency List : 
adj_list <- as_adj_list(g)

print_adj_list <- function(adj_list) {
  result <- list()
  for (node in names(adj_list)) {
    unique_connections <- unique(adj_list[[node]])
    result[[node]] <- unique_connections
  }
  return(result)
}

adjacency_list <- print_adj_list(adj_list)

# Display the first 5 nodes
cat("Adjacency List (First 5 Nodes ):\n")
print(adjacency_list[1:5])

# Save the adjacency list to a text file
writeLines(capture.output(print(adjacency_list)), "adjacency_list.txt")
cat("Adjacency list saved to 'adjacency_list.txt'\n")

### Edge List :
edge_list <- as.data.frame(get.data.frame(g, what = "edges"))

edge_list <- edge_list[, c("from", "to", "Rating")]

cat("First 5 rows of the edge list:\n")
print(head(edge_list, 5))

# Save the edge list to a CSV file
write.csv(edge_list, file = "edge_list.csv", row.names = FALSE)

cat("Edge list saved to 'edge_list.csv'\n")


# 2) Matrix Representation (Adjacency Matrix) :

adj_matrix <- as_adjacency_matrix(g, attr = "Rating", sparse = FALSE)
cat("Adjacency Matrix (First 20x20 Elements):\n")
print(adj_matrix[1:20, 1:20])

# Save the adjacency matrix to a CSV file
write.csv(as.data.frame(adj_matrix), file = "adj_matrix.csv", row.names = TRUE)

cat("Adjacency matrix saved to 'adj_matrix.csv'\n")
############################################################################
# Centrality measure

# Degree Centrality
V(g)$indegree <- strength(g, mode = "in", weights = E(g)$weight)
V(g)$outdegree <- strength(g, mode = "out", weights = E(g)$weight)

#Make sure K_in = K_out
sum(V(g)$indegree)
sum(V(g)$outdegree)

Node_in <- V(g)$indegree
Node_out <- V(g)$outdegree
hist(Node_in, col="lightblue", main="Degree In Centrality Distribution")
hist(Node_out, col="lightblue", main="Degree Out Centrality Distribution")

sum(V(g)$indegree) == sum(V(g)$outdegree)

# Transformation Weights
trans_weight <- E(g)$weight
trans_weight <- trans_weight - min(trans_weight) + 1
summary(trans_weight)

# Betweenness Centrality
V(g)$betweenness <- betweenness(g, directed=TRUE, weights=trans_weight)


# Closeness Centrality
V(g)$closeness <- closeness(g, mode = "all", weights = trans_weight)
summary(V(g)$closeness)
hist(V(g)$closeness, col="lightblue", main="Closeness Centrality Distribution")

# Eigenvector Centrality
V(g)$eigenvector <- eigen_centrality(g, directed = TRUE, weights = trans_weight)$vector
summary(V(g)$eigenvector)
hist(V(g)$eigenvector, col="lightblue", main="Eigenvector Centrality Distribution")

# PageRank Centrality
V(g)$pagerank <- page_rank(g, weights=trans_weight)$vector
hist(V(g)$pagerank, col="lightblue", main="PageRank Distribution")

# Top 5 nodes by each centrality measure
top_indegree <- head(sort(setNames(V(g)$indegree, V(g)$name), decreasing = TRUE), 5)
top_outdegree <- head(sort(setNames(V(g)$outdegree, V(g)$name), decreasing = TRUE), 5)
top_closeness <- head(sort(setNames(V(g)$closeness, V(g)$name), decreasing = TRUE), 5)
top_eigenvector <- head(sort(setNames(V(g)$eigenvector, V(g)$name), decreasing = TRUE), 5)
top_betweenness <- head(sort(setNames(V(g)$betweenness, V(g)$name), decreasing = TRUE), 5)
top_pagerank <- head(sort(setNames(V(g)$pagerank, V(g)$name), decreasing = TRUE), 5)

list(
  "Top In-Degree Nodes" = top_indegree,
  "Top Out-Degree Nodes" = top_outdegree,
  "Top Closeness Nodes" = top_closeness,
  "Top Eigenvector Nodes" = top_eigenvector,
  "Top Betweenness Nodes" = top_betweenness,
  "Top PageRank Nodes" = top_pagerank
)

############################################################################
#Connectivity
is_connected(g) 

is_connected(g, mode="strong")  
is_connected(g, mode="weak")  

edge_density(g)

components <- components(g)
print(components)

# Plot with connected components
plot(g, vertex.size = 5, edge.color = "black", vertex.label = NA,
     vertex.color = rainbow(components$no)[components$membership],
     mark.groups = split(1:vcount(g), components$membership),
     mark.col = rainbow(components$no, alpha = 0.3),
     mark.border = "black", main="All Components in the Graph")

#strong components
strong_components <- components(g, mode = "strong")
print(strong_components)

# Create a frequency table
freq_table <- table(strong_components$csize)
df_counts <- as.data.frame(freq_table)
df_counts

plot(g, vertex.size = 5, edge.color = "black", vertex.label = NA,
     layout = layout_with_fr(g, weights = trans_weight),
     vertex.color = rainbow(strong_components$no)[strong_components$membership],
     mark.groups = split(1:vcount(g), strong_components$membership),
     mark.col = rainbow(strong_components$no, alpha = 0.7),
     mark.border = "black", main="Strong Components in the Graph")

#largest component

largest_component <- induced_subgraph(g, which(components$membership == which.max(components$csize)))
is_connected(largest_component)  
is_connected(largest_component, mode = "strong")
is_connected(largest_component, mode = "weak")
plot(largest_component, vertex.size=5, vertex.label=NA, main="Largest Connected Component")

edge_density(largest_component)

largest_SCC <- induced_subgraph(g, which(strong_components$membership == which.max(strong_components$csize)))
plot(largest_SCC, vertex.size=5, vertex.label=NA, main="Largest strongly Connected Component")

edge_density(largest_SCC)

isolated_nodes <- V(g)[degree(g, mode = "total") == 0]
isolated_nodes

###############################################
#Special Graphs
#check if our network graph is a special type

# 1. is complete?
max_edges <- vcount(g) * (vcount(g) - 1)

# Check if the graph is complete
if (ecount(g) == max_edges) {
  print("The graph is complete.")
} else {
  print("The graph is not complete.")
}

# 2. is bipartite?
is_bipartite(g)

# 3. is tree?
is_tree(g)

# 4. is cycle?
is_dag(g)  #checks whether there is a directed cycle in the graph. If not, the graph is a DAG
############################################################################
#Quantifying network structure: 
### path length
Trans_weight <- E(g)$weight
Trans_weight <- Trans_weight - min(Trans_weight) + 1
E(g)$trans_weight <- Trans_weight

Inversd_weights <- 1/Trans_weight
E(g)$inversed_weights <- Inversd_weights

summary(E(g)$inversed_weights)  

dist_matrix <- distances(g, weights=E(g)$inversed_weights, algorithm = "Dijkstra")

mean(dist_matrix[!is.infinite(dist_matrix)])

summary(dist_matrix[!is.infinite(dist_matrix)])
length(dist_matrix[is.infinite(dist_matrix)]) #60448
zero_distances <- sum(dist_matrix == 0, na.rm = TRUE) # 3783 only self-loops 

hist(dist_matrix[!is.infinite(dist_matrix)], 
     breaks = 30, 
     col = "lightblue", 
     main = "Distribution of Shortest Path Lengths", 
     xlab = "Path Length")

## Diameter
graph_diameter <- diameter(g, weights = E(g)$inversed_weights)

#########################################################

### Clustering coefficient
avg_local_clustering <- transitivity(g, type = "average")
paste("Average Local Clustering Coefficient:", avg_local_clustering)

local_clustering <- transitivity(g, type = "local")
head(local_clustering) 
summary(local_clustering)
sum(is.na(local_clustering))

# Visualize node size based on local clustering coefficient
local_clustering[is.na(local_clustering)] <- 0  
plot(g, 
     vertex.size = local_clustering * 10,  
     vertex.label = NA, 
     edge.arrow.size = 0.5, 
     layout = layout_with_fr(g, weights = E(g)$trans_weight),
     main = "Node Size Based on Local Clustering Coefficient",
     )

# Plot histogram of local clustering coefficients
hist(local_clustering, 
     col = "lightblue", 
     main = "Distribution of Local Clustering Coefficients", 
     xlab = "Local Clustering Coefficient", 
     breaks = 30,  
     border = "white")

#############################################################
#sub graph
g_positive_in <- subgraph.edges(g, E(g)[E(g)$weight > 0 & .to(V(g))], delete.vertices = FALSE)
g_negative_in <- subgraph.edges(g, E(g)[E(g)$weight < 0 & .to(V(g))], delete.vertices = FALSE)

avg_local_clustering_positive <- transitivity(g_positive_in, type = "average")
avg_local_clustering_negative <- transitivity(g_negative_in, type = "average")

local_clustering_positive <- transitivity(g_positive_in, type = "local")
local_clustering_negative <- transitivity(g_negative_in, type = "local")

local_clustering_positive[is.na(local_clustering_positive)] <- 0
local_clustering_negative[is.na(local_clustering_negative)] <- 0

par(mfrow = c(1, 2))  

hist(local_clustering_positive, 
     col = "lightgreen", 
     main = "Nodes with Positive Ratings", 
     xlab = "Clustering Coefficient", 
     breaks = 30, 
     border = "white")

hist(local_clustering_negative, 
     col = "lightcoral", 
     main = "Nodes with Negative Ratings", 
     xlab = "Clustering Coefficient", 
     breaks = 30, 
     border = "white")

par(mfrow = c(1, 1))

########################################################################

library(visNetwork)

# calc avg weight for each node for color
avg_in_edge_weights <- sapply(V(g), function(v) {
  neighbors_v <- neighbors(g, v, mode = "in") 
  if (length(neighbors_v) == 0) {
    return(0)
  } else {
    avg_weight <- mean(E(g)[.from(neighbors_v)]$weight)
    return(avg_weight)
  }
})

summary(avg_in_edge_weights)
node_colors <- ifelse(avg_in_edge_weights >= 0, "lightblue", "salmon")

# calc avg trans weight for size (positive)
avg_trans_in_edge_weights <- sapply(V(g), function(v) {
  neighbors_v <- neighbors(g, v, mode = "in")  
  if (length(neighbors_v) == 0) {
    return(0) 
  } else {
    avg_trans_weight <- mean(E(g)[.from(neighbors_v)]$trans_weight)
    return(avg_trans_weight)
  }
})

summary(avg_trans_in_edge_weights)
node_size <- avg_trans_in_edge_weights * 10

# create data frame for nodes and edges for the visualization
nodes <- data.frame(id = V(g)$name, 
                    label = V(g)$name, 
                    size = node_size,
                    color = node_colors)

edges <- data.frame(from = as.character(ends(g, E(g))[,1]), 
                    to = as.character(ends(g, E(g))[,2]), 
                    weight = E(g)$weight)

head(nodes)
head(edges)

visNetwork(nodes, edges) %>%
  visNodes(size = 10) %>%
  visEdges(arrows = 'to', width = 0.5) %>%
  visIgraphLayout(layout = "layout_with_fr")%>% 
  visOptions(highlightNearest = TRUE)

################################################################

