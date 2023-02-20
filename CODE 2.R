# Load required packages
library(igraph)
library(dplyr)

# Load the data
normal_data <- read.csv("data/Breast-Cancer.csv") %>%
  sample_n(500) 
cancer_data <- read.csv("data/Breast-Normal.csv") %>%
  sample_n(500) 

# Combine the data
data <- rbind(normal_data, cancer_data)

# Create a graph object
graph <- graph_from_data_frame(data, directed = FALSE)

# Find the connected components
components <- clusters(graph)$membership

# Create a subgraph of the largest connected component
largest_component <- which(components == which.max(table(components)))
graph <- induced_subgraph(graph, largest_component)

# Set vertex attributes
V(graph)$color <- ifelse(V(graph)$name %in% normal_data$Protein.1, "lightblue", "red")
V(graph)$label.color <- "black"

# Set edge attributes
E(graph)$color <- ifelse(E(graph)$from %in% normal_data$Protein.1, "lightblue", "red")
E(graph)$width <- ifelse(E(graph)$from %in% normal_data$Protein.1, 2, 1)

# Set the layout
layout <- layout_with_fr(graph)

# Plot the graph
plot(graph, layout = layout)
