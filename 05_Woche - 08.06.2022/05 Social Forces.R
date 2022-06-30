#Fragen googeln (auf English)
#https://igraph.org/r/doc/
library(igraph)
library(igraphdata)


#Homophily
g=graph(edges=c("A","B", "A","C", "B","C", "B", "D","D","E"),
        directed = F)
plot(g)
V(g)
V(g)$group=c(1,1,1,2,2)
plot(g, vertex.color=V(g)$group)

vertex_attr(g)

homophily = function(g){
  label=V(g)$name
  V(g)$name=V(g)$group
  edges = get.data.frame(g)
  internal=length(which(edges$from == edges$to))
  external=length(which(edges$from != edges$to))
  internalprop = internal / (internal + external)
  col=ifelse(edges$from == edges$to, "black", "red")
  V(g)$name=label
  plot(g, vertex.color=V(g)$group, edge.color=col,vertex.size=20)
  return (c(internal,external,internalprop))
}

#Homophilie für den Karate Datensatz
data("karate")
plot(karate)
vertex_attr(karate)
V(g)$group = V(g)$color
homophily(g)

#zur Info: assortativity
?assortativity

#reciprocity
g = sample_gnp(3, 1, directed = T)
plot(g)
g[]
reciprocity(g)

g = sample_gnp(10, 0.3, directed = T)
plot(g)
dyad.census(g)
reciprocity(g)
mut=dyad.census(g)$mut
mut
ecount(g)
mut * 2 / ecount(g) #reciprocity

#Transitivity
?transitivity
g=graph(edges=c("A","B", "A","C", "B","C", "B", "D","D","E"),
        directed = F)
plot(g)
transitivity(g, type="local")
V(g)$name
transitivity(g, type="global")
?transitivity

plot(karate)
t=transitivity(karate, type="local")
t[is.nan(t)] = 1
t=1-t
plot(karate,vertex.size=b*20)

b=betweenness(karate,directed = F, normalized = T)
cor(b,t)

#Triaden-Census.
?triad.census
g = sample_gnp(5, 0.3, directed = T)
plot(g)
triad.census(g)

