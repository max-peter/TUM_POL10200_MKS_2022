library(igraph)
# This dataset contains a Facebook Ego-network taken 
# from https://snap.stanford.edu/data/egonets-Facebook.html
ego1 <- read.csv("698.edges", sep=" ", header=F) # please note the nodes being separated by a space character
head(ego1)
g <- graph_from_data_frame(ego1, directed=F)
plot(g, vertex.label=NA,vertex.size=3)
# important: the "ego" in the network is connected to 
# every other node and was removed!
g <- simplify(g, remove.multiple = T) # the dataset contains a little too much information for us
plot(g, vertex.label=NA,vertex.size=3)
#Components = unconnected parts
distances(g)
comp=components(g)$membership
comp
table(comp)
plot(g, vertex.color=comp,vertex.label=NA,vertex.size=10)
# This gives us the number of components in the graph, 
# the size of each component, and the membership of each 
# node to a component (from bottom to top). We can color 
# the nodes based on their component-membership
# what if we want to extract the largest component?
components(g)
components(g)$csize
max(components(g)$csize)
# which.max gives us the position of the maximum - in this case: the number of the largest component
largest <- which.max(components(g)$csize) 
largest
# Let's replicate the network keeping the vertices of the largest component only
g_largest = delete_vertices(g, 
            which(components(g)$membership != largest))
# delete_vertices creates a new graph from the old one
plot(g_largest,vertex.label=NA)
comp
#
# It seems like there are communities in the largest group
# how do we evaluate grouping in our network?
# The Newman/Girvan Grouping Algorithm is available via 
# cluster_edge_betweenness-function
communities <- cluster_edge_betweenness(g_largest)
# this gives an object listing resulting groups. this looks slighty different than a graph-object
communities

# the number of groups as well as the modularity of the graph is given. we can extract this information separately as well
modularity(communities)
length(communities) # the number of found groups
sizes(communities) # the size of each group

# and, as before, a list of membership for each node. 
membership(communities) 
# now, let's visualize the communities
plot(g_largest,vertex.color=membership(communities),vertex.label=NA)




#
# ÜBUNG BAYRISCHER LANDTAG
# Bayrischen Landtag laden (ausschuesse.txt)
# Umwandeln in 1-Mode Personen
# Wie viele Komponente? Visualisieren
# Communities errechnen und visualisieren
#
#

txt <- read.csv("ausschuesse.txt", sep="\t", header=T) # please note the nodes being separated by a space character
head(txt)
txt=txt[,1:2]
g <- graph_from_data_frame(txt, directed=F)
V(g)$type = bipartite.mapping(g)$type
plot(g,vertex.color=V(g)$type,vertex.label=NA)

?bipartite_projection
onemode = bipartite_projection(g, multiplicity = F, which=c("both"))
gPers = onemode$proj2
edge_attr(gPers)
plot(gPers, vertex.label=NA)

communities <- cluster_edge_betweenness(gPers, directed=F)
plot(gPers,vertex.color=membership(communities),vertex.label=NA)
length(communities)
modularity(communities)



#############################################################
