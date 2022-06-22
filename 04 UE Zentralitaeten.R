library (igraph)
txt = read.csv("min-bcd.txt", sep=",")
head(txt)
g = graph_from_data_frame(txt, directed=F)
plot (g)

#Die Position im Plot wird von einem Layout-Algorithmus bestimmt
l = layout_with_fr(g)
plot(g, layout=l)

#1. Degree Centrality
degree(g)
#Normalize
length(V(g))
degree(g) / (length(V(g))-1)

#iGraph?
deg = degree(g, normalized = T)
deg
ord=order(deg,decreasing = F)
ord
deg[ord]
barplot(deg[ord],horiz = T)
#top 5
ord=order(deg,decreasing = T)
barplot(rev(deg[ord[1:5]]),horiz = T)

#plot degree centrality
plot(g, layout=l,vertex.size = deg*60)

#Closeness Centralty
dists = distances(g)
dists
sum(dists[1,]) #summe der kürzesten Pfade für einen Knoten
rowSums(dists) # ... für alle Knoten
cc = 1 / rowSums(dists)
cc
cc / (1/ (length(V(g))-1)) # normalize
cc * (length(V(g))-1)
# iGraph
cc=closeness(g, normalized = T)
cc
plot(g, layout=l, vertex.size = cc * 50)

#Betweenness Centrality
bet = betweenness(g, normalized = T)
bet
plot(g, layout=l, vertex.size = bet*50)
#barplot in reihenfolge
ord=order(bet,decreasing = T)
barplot(rev(bet[ord]),horiz=T)

#Eigenvector Centrality
eig = eigen_centrality(g)
eig = eig$vector
plot(g, layout=l, vertex.size = eig * 30)

#Vergleiche alle Zentralitätswerte
layout(matrix(1:4, nrow=2))
par(mar=(c(0,4,2,2)))
plot(g, layout=l,vertex.size = deg*60, main="Degree")
plot(g, layout=l, vertex.size = cc * 50, main="Closeness")
plot(g, layout=l, vertex.size = bet*50, main="Betweenness")
plot(g, layout=l, vertex.size = eig * 30, main="Eigenvector")

layout(matrix(1:1, nrow=1))

#Netzwerkzentralisierungen
centr_degree(g, normalized = T)
centr_clo(g, normalized=T)
centr_betw(g, normalized = T)



#betweenness centrality der Personen der Ausschüsse
#zuerst diese aus 02 Übung laden und in 1-Mode projezieren
bet=betweenness(gPers, normalized = T)
bet=sqrt(bet)+0.01
#Parteifarben aus dem anderen Dokument wieder errechnen
plot(gPers, edge.width=E(gPers)$weight*1, vertex.size=bet*50,
     vertex.label=NA, vertex.label.color="black",
     vertex.color=persaus$farbe)

