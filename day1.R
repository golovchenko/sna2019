######################################################
########Creating your first network###################
######################################################

### loading packages
library(igraph)

### Creating an edgelist
# defining nodes
V2 <- c("Bob", "John", "Lisa", "Andrew")
V1  <- c("Yev", "Yev", "Yev", "Yev")

#puting everything together in one a dataframe
df <- data.frame(V1, V2)
df

### formatting as a graph object
g <- graph.data.frame(df)
E(g) #information about edges
V(g) #information vertices

### plotting the network
plot.igraph(g)


### turning the graph object into an adjacency matrix
matrix <- as_adjacency_matrix(g)
matrix

### turning the matrix into a graph object
g1 <- graph_from_adjacency_matrix(matrix, mode = "undirected")

### plotting the graph object
plot.igraph(g1)


######################################################
########Creating your first network###################
######################################################

###Did the abovementioned individuals take courses at the departpment of Political Science at KU during their bachelor? 
V1 <- c("Bob", "John", "Yev")
V2 <- c("Bachelor", "Bachelor", "Bachelor")

### formatting as a dataframe edgelist
df.affil <- data.frame(V1, V2)
df.affil

### formatting as a graph object
g2 <- graph.data.frame(df.affil, directed = F)

##adding node attribute indicating the type of the node
V(g2)$type <- bipartite.mapping(g2)$type

## adding colors to show profile type (individual/event)
colors <- V(g2)$type

### Plotting bi-partite affiliation network
plot.igraph(g2, main="Bi-partite network", vertex.color = colors)

### Plotting one-mode affiliation network
plot.igraph(bipartite_projection(g2)$proj1,main="Affilitaton Network")

### Plotting one-mode affiliation-affiation network
plot.igraph(bipartite_projection(g2)$proj2,main="Affilitaton Network")

