# Load igraph package to calculate parameter that implement in igraph
library(igraph)

# load data and get igraph object
graph_object = readGraph("data/Colon-Cancer.csv")
subgraph_object = readSubgraph("data/Kidney-Cancer.csv")

# find number of vertecies and edges in graph
graph_vertex_count <- vcount(graph_object)
graph_edge_count <- ecount(graph_object)

print(paste0("Graph Vertex Count: ",graph_vertex_count))
print(paste0("Graph Edge Count: ",graph_edge_count))

# find number of vertecies and edges in subgraph
subgraph_vertex_count <- vcount(subgraph_object)
subgraph_edge_count <- ecount(subgraph_object)

print(paste0("Subraph Vertex Count: ",subgraph_vertex_count))
print(paste0("Subraph Edge Count: ",subgraph_edge_count))

# calculate closeness centrality for each node
# for print closeness of each nodes use 'print(closeness)'
closeness <- closeness(subgraph_object,weights = NULL)

# Reduse function of base in R, that calculate a operator in a input list
# paste0 function use to print a text with variable
print(paste0("Closeness Centrality: ",Reduce("+",closeness)))


# Connectivity that divide to edge and vertex connectivity
# calculate edge connectivity, another function is vertex_connectivity
print(paste0("Vertex Connectivity: ",cohesion(subgraph_object)))
# calculate edge connectivity, another function is edge_connectivity
print(paste0("Edge Connectivity: ",adhesion(subgraph_object)))

# calculate Diameter of graph
GD <- diameter(graph_object)
print(paste0("Graph Diameter: ",GD))

# calculate Radius of graph
GR <- radius(graph_object)
print(paste0("Graph Radius: ",GR))

# calculate of Index of Aggregation
IoA <- (subgraph_vertex_count / graph_vertex_count)
print(paste0("Index of Aggregation: ",IoA))

# calculate of Connectivity
A <- graph_edge_count
B <- (graph_vertex_count*(graph_vertex_count-1))/2
C <- A/B
print(paste0("Connectivity: ",C))

# calculate of Graph Centrality (Eccentrisity)
print(paste0("Graph Centrality: ",mean(1/eccentricity(graph_object))))

# calculate of Number of edges divided by the number of vertices
NeNv <- subgraph_edge_count/subgraph_vertex_count
print(paste0("Number of edges divided by the number of vertices: ",NeNv))

# Degree Centrality Distribution, calculate mean of Degree.
# we can show degree distribution with table(graph_degree)
# show plot of Degree Distribution and Mean: plot(graph_degree, col="red")
graph_degree <- degree(graph_object)
mean_graph_degree <- mean(graph_degree)
print(paste0("Mean of Graph Degree: ",mean_graph_degree))

# Cluster Coefficient
avrageClusterCoefficient = mean(transitivity(graph_object, type = "localundirected", weights = NULL,isolates = "zero"))
print(paste0("Cluster Coefficient: ", avrageClusterCoefficient ))

# Betweenness Centrality
betweennessMean = mean(betweenness(graph_object,weights = NULL))
print(paste0("Betweenness: ", betweennessMean ))

#Subgraph Centrality
print(paste0("Subgraph Centrality: ",mean(subgraph_centrality(graph_object))))

# Hub
table_graph_degree <- graph_degree
sum_of_hub_5 <- sum(table_graph_degree > 5)
sum_of_hub_8 <- sum(table_graph_degree > 8)
sum_of_hub_12 <- sum(table_graph_degree > 12)
sum_of_hub_20 <- sum(table_graph_degree > 20)
print(paste0("Count of Hub(Degree > 5): ",sum_of_hub_5))
print(paste0("Count of Hub(Degree > 8): ",sum_of_hub_8))
print(paste0("Count of Hub(Degree > 12): ",sum_of_hub_12))
print(paste0("Count of Hub(Degree > 20): ",sum_of_hub_20))

# Visualize the network
plot(graph_object, vertex.size=5, vertex.label=NA)

# Set labels for the vertices
vertex_labels <- V(graph_object)$name

# Visualize the network with labels
plot(graph_object, vertex.size=5, vertex.label=vertex_labels)



# Some more Visualization

# plot the degree distribution
hist(graph_degree, breaks=20, main="Degree Distribution", xlab="Degree")

# plot the betweenness centrality
plot(betweenness(graph_object, normalized=TRUE), main="Betweenness Centrality", xlab="Node ID", ylab="Centrality")

# plot the subgraph centrality
plot(subgraph_centrality(graph_object), main="Subgraph Centrality", xlab="Node ID", ylab="Centrality")

# plot the clustering coefficient
plot(transitivity(graph_object, type="local"), main="Clustering Coefficient", xlab="Node ID", ylab="Coefficient")

# plot the network with node size proportional to degree
plot(graph_object, vertex.size=graph_degree*0.5, vertex.label=NA, main="Network with Node Size Proportional to Degree")

# plot the network with node color proportional to degree
plot(graph_object, vertex.size=5, vertex.label=NA, vertex.color=graph_degree, main="Network with Node Color Proportional to Degree")

# Set the color of nodes in subgraph to red
V(subgraph_object)$color <- "red"
# Plot the subgraph within the original graph
plot(graph_object, vertex.size=5, vertex.label=NA)
plot(subgraph_object, add=TRUE)

# Set the node size to be proportional to the node's degree
node_sizes <- degree(graph_object)
# Plot the graph with node size proportional to degree
plot(graph_object, vertex.size=node_sizes, vertex.label=NA)

# Set the edge weights to be the reciprocal of their distance
edge_weights <- 1/ecount(graph_object)
# Plot the graph with edge widths proportional to edge weights
plot(graph_object, edge.width=edge_weights, vertex.label=NA)

