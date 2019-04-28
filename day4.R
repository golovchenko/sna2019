
## setting the work directory
setwd("ISERT/YOUR/WORKING/DIRECTORY/HERE/")

### loading the packages
library(dplyr)
library(igraph)
## loading the data
df <- read.csv("data/DIPCON_3.0_Dyads.csv")

### using dplyr to keep only relations for 2010
df1 <- df %>% # selecting th data set
  filter(dipcon2010 > 0) %>% # keeping only the connections in 2010
  select(abbrev1, abbrev2) # selecting the variables


#Turning into a graph object
g <- graph.data.frame(df1, directed = F) # storing the graph as "undirected"

## Simplifying the graph
g <- simplify(g, remove.multiple = T, remove.loops = T)#use this function to remove multiples or self-ties 


#Exercise 1 
mean(degree(g))

graph.density(g)


### vizualising the network
plot.igraph(g,
            layout=layout.fruchterman.reingold,
            vertex.label = NA,
            vertex.size = 5,
            edge.size = 0.01,
            edge.color="grey",
            edge.curved= .2,
            vertex.frame.color="#ffffff",
            main='Diplomatic relations between states - 2010')




### computing centralization
deg<- centr_degree(g, normalized = T)

names(deg)
deg$centralization

### Exercise 2

between <- centr_betw(g, normalized = T)
between$centralization

# outputting normalized closeness centrality 
closeness <- centr_clo(g, normalized = T)
closeness$centralization


between <- centr_betw(g, normalized = T)
between$centralization

closeness <- centr_clo(g, normalized = T)
closeness$centralization



add <- function(a,b){
  c <- a+b
  c}


### Exercise 3: Creating a function
cent <- function(data, year){
  data1 <- data %>% 
    filter(year > 0) %>% 
    select(abbrev1, abbrev2)
  graph <- graph.data.frame(data1, directed = F)
  graph <- simplify(graph, remove.multiple = T, remove.loops = T) #use this
  deg1<- centr_degree(graph, normalized = T)
  deg1$centralization}



### degree centralisation for a given year
dip70 <- cent(df, df$dipcon1970)
dip75 <- cent(df, df$dipcon1975)
cdip <- c(dip70, dip75)# storing as a datframe
dip.df <- data.frame(cdip)



# alternative
dip.df <- data.frame(cdip = c(dip70, dip75))
#alternative
head(dip.df)
cdip


### Excersise 4
dip80 <- cent(df, df$dipcon1980)
dip90 <- cent(df, df$dipcon1990)
dip00 <- cent(df, df$dipcon2000)
dip10 <- cent(df, df$dipcon2010)

### creating a dataframe with years and degree centralization 
c.dip <- c(dip80, dip90, dip00, dip10)
year <- c(1980, 1990, 2000, 2010)
c.df <- data.frame(year, c.dip)
head(c.df)



### ggplot 
library(ggplot2)

ggplot(c.df, aes(year, c.dip)) + geom_line()

library(ggplot2)
ggplot(c.df, aes(year, c.dip)) +
  geom_line()+
  labs(x = "Year", y = "Degree centralization")

### exporting the graph to your working directory
ggsave("world_network.png", plot = last_plot(),
       width = 25, height = 10, units = "cm",
       dpi = 1000)


### Computing node-level centrality - recap
df.g <- data.frame(country = V(g)$name,
                   degree = degree(g, mode = 'all'),
                   betweenness = betweenness(g, directed = F, normalized = T),
                   closeness = closeness(g, mode = "all", normalized = T))


### Exercise 5
## creating a scatterplot to explore the relationship between degree centrality and betweeness centrality
ggplot(df.g, aes(degree, betweenness)) +  geom_point()

## creating a dataframe with centrality scores for each country
ggplot(df.g, aes(degree, betweenness, size = closeness)) +
  geom_point()


## Addding dimensions

## creating a variable indicating wether the degree is below or above 100
df.g$deg_group[df.g$degree <100] <- "Less 100 ties"  
df.g$deg_group[df.g$degree >= 100] <- "At least 100 ties"
### creating a dataframe with centrality scores for each country
ggplot(df.g, aes(degree, betweenness, size = closeness,
                 colour = deg_group)) +
  geom_point() 


### removing clutter by applying Edward Tufte's theme
library(ggthemes)
ggplot(df.g, aes(degree, betweenness, size = closeness,
                 colour = deg_group)) +
  geom_point() +
  theme_tufte()


