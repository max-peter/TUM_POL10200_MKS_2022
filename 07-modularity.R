library(igraph)
g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
plot(g)
g <- add_edges(g, c(1,6, 1,11, 6, 11))
lay <- layout_with_fr(g)
plot(g,layout=lay)
wtc <- cluster_edge_betweenness(g)
wtc
part=membership(wtc)
plot(g,vertex.color=part,layout=lay)
modularity(g, part)
get.adjacency(g)

modularity_explained = function(netw,communities){
  m = length(E(netw))
  n = length(V(netw))
  deg = degree(netw)
  adjmatrix = get.adjacency(netw)
  Q = 0
  
  for (i in 1:n){
    for (j in 1:n){
      if (communities[i]==communities[j]){
        Q = Q + adjmatrix[i,j]-deg[i]*deg[j]/(2*m)
        
        # adjmatrix[i,j] is 1 if i and j are connected, 0 otherwise
        
        # deg[i]*deg[j]/(2*m)
        # probability that an edge exists between vertices i and j if edges are created at random but respecting the vertex degrees
        
        # difference between the two:
        # if i and j are not connected, modularity should decrease by the above-mentioned probability 
        # if i and j are connected and an edge between them falls in the same community modularity should increase. 
        # 1-probability is larger for pairs of nodes with small degrees
      }
    }
  }
  Q = Q/(2*m)
  # 2m is twice the number of edges
  # doubling is needed since we visit every pair of nodes twice
  return (Q)
}


modularity(g, part)
modularity_explained(g,part)

#im vergleich ein zufallswert für die Gruppen
part_random <- floor(runif(vcount(g),min=1,max=4))
part
part_random
plot(g,vertex.color=part_random,layout=lay)
modularity_explained(g,part_random)
