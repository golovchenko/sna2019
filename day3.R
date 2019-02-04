### clearing memory
rm(list=ls())




## setting the work directory
setwd("C:/Users/htw606/Documents/2018 09 08 Universitet/Disinfo/Logistics/Own course/Courses 2018 info/SNA/scripts")


###########################################################################
###############Using Youtube's API from R##################################
###########################################################################
install.packages("tuber")
library("tuber") #YouTube API V3.
yt_oauth(app_id = "751322083724-f0j17h4umtcnvqnhe907lp0ger2s9jbl.apps.googleusercontent.com",
         app_secret = "Vxg9be1nV4105HSfPG-r4-CE", token = '')

## download video details 
pepe_video <- get_video_details("7Q5HK9HsBSU")
pepe_video$items

## downloading list of channels that the channel in question subscribed to
pepe_channel <- get_subscriptions(filter = c(channel_id = "UCN3iJoH8x_-rGlmj_UmKACQ"))
class(pepe_channel)
pepe_channel$items


###########################################################################
###################Importing exporting data as graphml ####################
###########################################################################


library(igraph)

#exporting the rts.g graph object as a graphml file 
write.graph(rts.g, file="your_file.graphml", format="graphml") #rts.g is your graph file


### you can import the network file in gdf format from Youtube Data Tools using readgdf() 
### Alternatively, you can covert the gdf file to graphml by going through the following steps:
#1  Read the gdf file in Gephi
#2 "Export" the network as graphml file into your working directory
#3  Load the graphml file in R using the code below

##reading a graphml file in your working directory as a graph object
g_import <- read.graph("your_file_in_the_working_dirctory.graphml", format = "graphml") 

### test run 
plot.igraph(g_import)





###########################################################################
#######################Analyze the NBC News troll data ####################
###########################################################################

### clearing memory
rm(list=ls())


## setting the work directory
setwd("INSERT THE PATH FOR YOUR WORKING DIRECTORY HERE")

#loading packages
library("dplyr") #for data manipulation
library("igraph") # for social network analysis

### set the working directory

tweets <- read.csv("http://nodeassets.nbcnews.com/russian-twitter-trolls/tweets.csv",
                   stringsAsFactors = F, sep = ",")

#selecting only the retweets
rts <- grep("^rt @[a-z0-9_]{1,15}", tolower(tweets$text), perl=T, value=T)
# extracting handle names for the senders (those who retweet)
rt.sender <- tolower(as.character(tweets$user_key[grep("^rt @[a-z0-9_]{1,15}", tolower(tweets$text), perl=T)]))
# extracting handle names for the recievers (those who are being retweeted)
rt.receiver<- tolower(regmatches(rts, regexpr("@(?U).*:", rts)))
rt.receiver <- (gsub(":", "", rt.receiver)) #removing ":"
rt.receiver <- (gsub("@", "", rt.receiver)) #removing "@"
### Registering empty entries as missing
rt.sender[rt.sender==""] <- "<NA>"
rt.receiver[rt.receiver==""] <- "<NA>"
# a large proportion of this code is from <https://www.r-bloggers.com/generating-graphs-of-retweets-and-messages-on-twitter-using-r-and-gephi/> 

#storing reciever and sender handle names in one dataframe and removing duplicates
handle.all <- unique(as.data.frame(c(rt.sender, rt.receiver))) 
#renaming the handle names variable
handle.all <- handle.all %>% rename(handle = "c(rt.sender, rt.receiver)")


# importing handle names from the official list release in congress
trolls_official <-  read.csv("http://golovchenko.github.io/data/trollhandles.txt", stringsAsFactors = F)
# merging the complete list of official troll handle names with the ones in NBC data
tweets <- tweets %>% rename(handle = user_key) #renaming handle name variable
handles <- tweets %>% select(handle) #selecting only the handles from the data
handles <- rbind(trolls_official, handles)
handles.u <- unique(handles) #removing duplicates
handles.u$troll <- "troll" #assigning all of these users a trolls
### matching trolls with the complete set of handle names in the retweet network
nodes <- right_join(handles.u, handle.all)
nodes <- replace(nodes, is.na(nodes), "non-troll") # now we have a variable indicating wether a user is a troll


### Creating a data frame from the sender-receiver objects
rts.df <- data.frame(rt.sender, rt.receiver)
### creating the retweetnetwork based on the sender-receiver df and the node attributes (troll/non-troll)
rts.g <- graph.data.frame(rts.df, directed=T, vertices = nodes)
### removing self-ties
rts.g <-simplify(rts.g, remove.loops = T, remove.multiple = F)

### creating the retweetnetwork based on the sender-receiver df and the node attributes (troll/non-troll)
rts.g <- graph.data.frame(rts.df, directed=T, vertices = nodes)
### removing self-ties
rts.g <-simplify(rts.g, remove.loops = T, remove.multiple = F)

# removing multiple edges between users
g <- simplify(rts.g, remove.multiple = T, remove.loops = T)
# creating a data frame with weighted and unweighted degree centrality for each profile
df <- data.frame(name =V(g)$name,
                 troll= V(g)$troll,indegree=degree(g,mode='in'),
                 indegree_weighted = degree(rts.g, mode ="in"),
                 outdegree=degree(g,mode='out'),
                 outdegree_weighted = degree(rts.g, mode = "out"))
#ranking users by indegree
rank.indegree <- df %>% select(name, troll, indegree,
                               indegree_weighted) %>% arrange(-indegree)


#ranking users b weigted indegree n users * n retweets
rank.indegree.w <- df %>% select(name, troll, indegree,
                                 indegree_weighted) %>% arrange(-indegree_weighted)
install.packages("knitr")
library(knitr)

### Top 10 profiles ranked by indegree
kable(rank.indegree[1:10,], caption = "Top 10 profiles ranked by indegree")

### Top 10 profiles ranked by weighted indegree
kable(rank.indegree.w[1:10,], caption = "Top 10 profiles ranked by weighted indegree")

### subsetting the graph by removing non-trolls
#selecting nodes to exclude
exclude <- V(rts.g)[troll == "non-troll"]
#excluding the nodes
g.troll <- delete.vertices(rts.g, exclude)

### vizualizing the graph
par(bg ="grey10")
plot.igraph(g.troll,layout= layout.fruchterman.reingold(g.troll),
            edge.color="grey",
            edge.curved= .2, vertex.label = NA, vertex.frame.color="#ffffff",
            vertex.size = 2, edge.size = 0.01, edge.arrow.size = 0.01)



#decomposing the graph into components and returning the largest one
comp <- decompose(g.troll, mode = c("weak"), max.comps = 1,
                  min.vertices = 1)
### plotting the graph
par(bg ="grey10")
plot.igraph(comp[[1]],layout= layout.fruchterman.reingold(comp[[1]]),
            edge.color="grey",
            edge.curved= .2, vertex.label = NA, vertex.frame.color="#ffffff",
            vertex.size = 4, edge.size = 0.005, edge.arrow.size = 0.01)


#exporting the rts.g graph object as a graphml file 
write.graph(rts.g, file="troll_network.graphml", format="graphml")


### g1 <- read.graph("data/k10.graphml", format = "graphml")

