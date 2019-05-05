### setting the working directory
setwd("Your/Working/Directory/Path")

### loading packages
library(dplyr)
library(igraph)
library(ggplot2)


#########################################################################
#################################Cliques ################################
#########################################################################

### mockup data set and most of the code from https://stackoverflow.com/questions/36276433/igraph-k-core-with-graph-coreness-underestimating-some-cores
data <- matrix(c("Ns-1","Ns-1","Ns-1","Ns-1","Ns-1","Ns-1","Ns-1","Ns-1","Ns-14","Ns-14","Ns-15","Ns-15","Ns-15","Ns-17","Ns-17","Ns-17","Ns-17","Ns-2","Ns-2","Ns-2","Ns-2","Ns-4","Ns-4","Ns-4","Ns-5","Ns-5","Ns-5","TAMU-7","Ns-14","Ns-15","Ns-17","Ns-2","Ns-4","Ns-5","TAMU-7","TAMU-8","Ns-15","Ns-17","Ns-17","Ns-4","Ns-18","Ns-2","Ns-4","Ns-5","Ns-18","Ns-4","Ns-5","TAMU-7","TAMU-8","Ns-5","TAMU-7","TAMU-8","TAMU-7","TAMU-8","Ns-18","TAMU-8"),nrow=28,ncol=2)

### creating a graph object
graph <- graph.edgelist(data[,1:2],directed=F)


### plotting the graph 
plot(graph,
     vertex.size = 20,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = "white")

### identifying cliques
cliques <- cliques(graph, min =5) #mininum clique size = 5 nodes

## how many cliques are there?
length(cliques)

### examining clique members
cliques

### clique size
sapply(cliques(graph, min = 5), length) 

###  identifying the largest clique
largest_cliques(graph) 

### adding grey collor to all of the nodes
vcol <- rep("grey80", vcount(graph))

## adding orange colour to the largest clique
vcol[unlist(largest_cliques(graph))] <- "orange"


### vizualising who is in the largest clique (orange) and who is not (grey)
plot(graph,
     vertex.size = 20,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = vcol)

# for more info om vizualisation, see this blog http://kateto.net/networks-r-igraph

### to compute n-cliques and n-clans see the stack oveflow thread: https://stackoverflow.com/questions/40088150/find-n-cliques-in-igraph



#########################################################################
#################################k-cores ################################
#########################################################################

### as a rule of thumb, you should first choose the component before computing k-cores. 
###In this dataset there is only one component. if there are more, use the example below to select the largest component 
### see documentation for decompose() before using
#comp <- decompose(graph, mode = c("weak"), max.comps = 1, min.vertices = 1)
#plot.igraph(comp)


### computing graph coreness
V(graph)$Kcore = graph.coreness(graph)

# examining the graph
plot(graph,
     main="K Cores",
     vertex.size = 20,
     vertex.label = V(graph)$Kcore,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = "white")

# storing a list of nodes that belong to k <=3
exclude <- V(graph)[Kcore < 4]

# removing these nodes in order to make a k-4 core
k4 <- delete.vertices(graph, exclude)

##

### plotting k4
plot(k4,
     main="K Cores",
     vertex.size = 20,
     vertex.label = V(k4)$Kcore,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = "white")

### plotting k4 where color reflects k
plot(k4,
     main="K Cores",
     vertex.size = 20,
     vertex.label = V(k4)$Kcore,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = V(graph)$Kcore)



### plotting k4 where node size reflects
#computing degree
deg <- degree(k4)
plot(k4,
     main="K Cores",
     vertex.size = deg, # node size reflects degree
     vertex.label = V(k4)$Kcore,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = V(graph)$Kcore)



















#########################################################################
###### Analyzing k-cores in DIPCON dataset for 1970 ##############
#########################################################################

## loading the data
df <- read.csv("data/DIPCON_3.0_Dyads.csv") # data and documentation from: http://www.u.arizona.edu/~volgy/data.html

### using dplyr to keep only relations for 1970
df70 <- df %>% # selecting th data set
  filter(dipcon1970 > 0) %>% # keeping only the connections in 1970
  select(abbrev1, abbrev2) # selecting the variables

#Turning into a graph object
g70 <- graph.data.frame(df70, directed = F) # storing the graph as "undirected" for the sake of simplicity

## Even though the graph object is sstored as "undirected", some edges may appear twice (in cases where reciprocal ties are directional)
g70 <- simplify(g70, remove.multiple = T, remove.loops = T) #use this function to remove multiples or self-ties 

### identifying k-cores
V(g70)$Kcore = graph.coreness(g70)

### vizualising the network
plot.igraph(g70,
     main="K Cores",
     vertex.size = 2,
     vertex.label = V(g70)$Kcore,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = V(g70)$Kcore)




#########################################################################
###### Analyzing k-29 in DIPCON dataset for 1970 ##############
#########################################################################


### storing countries outside of the k-29 core
exclude1 <- V(g70)[Kcore < 29]
# removing these nodes in order to make a k-29 core
k29 <- delete.vertices(g70, exclude1)

## plotting k29
plot.igraph(k29, 
            main = "K-29 core",
            vertex.size = 2)

## exploring the degree distribution for k29
deg29 <- degree(k29)
deg29



#########################################################################
####Ranking countries by their position in the core-periphery############
#########################################################################

### storing country name and coreness in a dataframe
df.70 <- data.frame(country = V(g70)$name,
                    Kcore = V(g70)$Kcore, 
                    degree = degree(g70))

### Keepingonly countries from the K-29 core
library(dplyr)
df.core <- df.70 %>%
  filter(Kcore >= 29) %>%
  arrange(-degree) # arranging by the degree 

### printing out top 50 countries from the k-29 core ranked by degree 
head(df.core, 50)


### Keeping only countries from OUTSIDE the K-29 core
df.periphery <- df.70 %>%
  filter(Kcore < 29) %>%
  arrange(-degree) # arranging by the degree 


### printing out top 50 countries from outside the k-29 core ranked by degree 
head(df.periphery, 50)







#########################################################################
#################################Bonus: Cliques ################################
#########################################################################

### plotting the graph 
plot(graph,
     vertex.size = 20,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = "white")


### identifying cliques
cliques <- cliques(graph, min =5) #mininum clique size = 5 nodes

## how many cliques are there?
length(cliques)

### examining clique members
cliques

### clique size
sapply(cliques(graph, min = 5), length) 

###  identifying the largest clique
largest_cliques(graph) 

### adding grey collor to all of the nodes
vcol <- rep("grey80", vcount(graph))

## adding orange colour to the largest clique
vcol[unlist(largest_cliques(graph))] <- "orange"


### vizualising who is in the largest clique (orange) and who is not (grey)
plot(graph,
     vertex.size = 20,
     vertex.label.color = "black",
     vertex.label.cex = 1,
     vertex.color = vcol)

# for more info om vizualisation, see this blog http://kateto.net/networks-r-igraph

### to compute n-cliques and n-clans see the stack oveflow thread: https://stackoverflow.com/questions/40088150/find-n-cliques-in-igraph


