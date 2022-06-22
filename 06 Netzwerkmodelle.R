library(igraph)
# Graph generators

# Erdos-Renyi Random Graphs
?sample_gnm
g = sample_gnm(100, 400)
plot(g)
degree(g)
mean(degree(g))
hist(degree(g))

table(degree(g))
barplot(table(degree(g))) # degree distribution

mean(distances(g)) # average shortest path
transitivity(g, type="global") # clustering coefficient


# Watts-Strogatz Small World Model
g = sample_smallworld(dim=1, 100, nei=4, p=0.0)
plot(g,layout=layout_in_circle(g),vertex.label=NA,vertex.size=1)
neighbors(g,10)
mean(degree(g))
barplot(table(degree(g))) # degree distribution
mean(distances(g))
transitivity(g, type="global") # clustering coefficient

g = sample_smallworld(dim=1, 100, nei=4, p=0.05)
plot(g,layout=layout_in_circle(g),vertex.label=NA,vertex.size=1)
mean(distances(g))
transitivity(g, type="global") # clustering coefficient
mean(degree(g))

#
### small world properties
g <- sample_smallworld(dim=1, 1000, nei=3, p=0)
plot(g, layout=layout_in_circle(g),vertex.label=NA,vertex.size=3)

rewiring = function(g){
  n1=sample(V(g),1)
  ns=neighbors(g,n1)
  n2=sample(ns,1)
  str=paste(n1,"|",n2,sep="")
  g=delete.edges(g,str)
  n1=as.integer(n1)
  ns=neighbors(g,n1)
  nneu=sample(length(V(g)),1)
  g=add_edges(g,c(n1,nneu))
  ns=neighbors(g,n1)
  return (g)
}
myplot = function(g){
    plot(g, layout=layout_in_circle(g),vertex.label=NA,vertex.size=1)
}

g <- sample_smallworld(dim=1, 1000, nei=3, p=0)
dd=c()
tt=c()
for (i in 1:300){
  d=mean(distances(g))
  dd=c(dd,d)
  t=transitivity(g, type="global")
  tt=c(tt,t)
  g=rewiring(g)
}
plot(dd/max(dd),type="l")
lines(tt/max(tt),col=2)
###
barplot(table(degree(g)))


# Scale-Free Network according to the Barabasi-Albert-Model
?sample_pa
g <- sample_pa(100, m=2, directed=F)
plot(g, layout=layout_with_fr(g),vertex.size=degree(g),vertex.label=NA)
barplot(table(degree(g))) # degree distribution
mean(distances(g)) # average shortest path
transitivity(g, type="global") # clustering coefficient
mean(degree(g))
