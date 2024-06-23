#' calculates centrality measures, detects communities within the network
#' assesses network modularity, identifies the most central node
#' generates visualizations to highlight these aspects

#load necessary libraries
library(igraph)
library(tidytext)
library(dplyr)
library(ggraph)
library(ggplot2)

comments <- read.csv("./assets/antiwork.csv")

#tokenize comments into words
comments_words <- comments %>%
    unnest_tokens(word, comment)

#create edges based on word co-occurrence within comments
edges <- comments_words %>%
    group_by(comment_id) %>%
    pairwise_count(word, sort = TRUE)

#create a graph from the edges
network <- graph_from_data_frame(edges, directed = FALSE)

#calculate centrality measures
degree_centrality <- degree(network, mode = "all")
closeness_centrality <- closeness(network, mode = "all")
betweenness_centrality <- betweenness(network, directed = FALSE)

#detect communities
communities <- cluster_louvain(network)

#calculate network density
network_density <- edge_density(network)

#basic network plot
plot(network, vertex.size = sqrt(degree_centrality) * 2, asp = FALSE)

#plot with community detection
plot(communities, network, asp = FALSE, vertex.size = sqrt(degree_centrality) * 2)

#enhance visualization using ggplot2
ggraph(network, layout = "fr") +
        geom_edge_link() +
        geom_node_point(aes(size = degree_centrality)) +
        theme_graph() +
        ggtitle("Raniwork Comments Network Analysis")

##additional features

#calculate modularity of the network to assess the strength of divisions into communities
network_modularity <- modularity(communities)
print(paste("Network Modularity:", network_modularity))

#identify the most central node in the network
most_central_node <- which.max(degree_centrality)
print(paste("Most Central Node:", V(network)[most_central_node]$name))

#visualize the network with modularity highlighting
ggraph(network, layout = "fr") +
        geom_edge_link() +
        geom_node_point(aes(size = degree_centrality, color = as.factor(membership(communities)))) +
        scale_color_viridis_d() +
        theme_graph() +
        ggtitle("Raniwork Comments Network Analysis with Community Highlighting")

#export the network visualization to a file
ggsave("raniwork_network_analysis.png")