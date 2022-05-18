# Danke an Katherine Ognyanova (https://kateto.net/tutorials/)

library(igraph)
# Einfache Netzwerke in R erzeugen
g = graph(edges=c(1,2, 2,3, 3,1), directed=F)
plot(g)
class(g)

g = graph(edges=c(1,2, 2,3, 3,1), n=10) #mit Exraknoten
plot(g)
g

#Netzwerk mit Labels erzeugen
g = graph(edges=c("Anton","Berta", "Berta","Dora", "Dora","Anton"),
          directed=F)
plot(g)

#und Isolate hinzufügen
g = graph(edges=c("Anton","Berta", "Berta","Dora", "Dora","Anton"),
          isolates=c("Emil","Friedrich","Gustav"),
          directed=F)
plot(g)

V(g) #Liste der Knoten
E(g) #Liste der Kanten

g[] #Matrix
g[1,6]=1
g[]
plot(g)

#Die Liste der Knoten hat Attribute
attributes(V(g))
V(g)$gender=c(2,1,1,2,2,2)
vertex_attr(g)
plot(g,vertex.color=c("pink","skyblue"))        

#leeres Netzwerk erzeugen
g = make_empty_graph(100)
plot(g)

#Zufallsnetzwerk erzeugen
g = sample_gnm(100,200)
plot(g)


# Netzwerke einlesen (edgelists)
# Wie erzeuge ich diese edgelists? Am besten in Excel

txt = read.csv("03 TestNetzwerk.txt", sep="\t", encoding = "UTF-8")
txt
g = graph_from_data_frame(txt, directed = F)
plot(g)

#Das Spaltentrennzeichen (sep) ist wichtig!
txt = read.csv("simple.csv", sep=",", encoding = "UTF-8")
txt
g = graph_from_data_frame(txt, directed = T)
plot(g)

# Importieren anderer Netzwerk Dateiformaten
g = graph("Zachary")
?graph

#Eine Pajek Netzwerkdate imoportieren
g=read.graph("CEOs.net",format="pajek")
plot(g)
