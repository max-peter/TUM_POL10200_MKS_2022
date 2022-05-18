library(igraph)
# encoding = "UTF-8" !!!
txt = read.csv("ausschuesse.txt", sep="\t", encoding = "UTF-8")
head(txt)
dim(txt)
g = graph_from_data_frame(txt, directed = F)
g = simplify(g, remove.multiple = T, remove.loops = T)
V(g)$name
V(g)$color = c(rep("orange",14) , rep("lightblue",vcount(g)-14))
plot(g, vertex.size=5, vertex.label=NA)

# Dieses Netzwerk ist ein 2-Mode Netzwerk -> Personen und Gremien
# Zuordnung zu Modes (=Farben) sollte "automatisch" geschehen

vertex_attr(g)
g = delete_vertex_attr(g, "color")
vertex_attr(g)

plot(g, vertex.size=5, vertex.label=NA)

################
# wir können die Anzahl der Knoten ermitteln (unique)
head(txt)
nPersonen = length(unique(txt$MdL))
nAusschuss = length(unique(txt$Ausschuss))
V(g)$color = c(rep("orange",nAusschuss) , rep("lightblue",nPersonen))
plot(g, vertex.size=5, vertex.label=NA)
####

# Netzwerklösung
g = delete_vertex_attr(g, "color")
bipartite.mapping(g)
V(g)$type = bipartite.mapping(g)$type
vertex_attr(g)

# Zurodnung von Shape und Color über Type
V(g)$color = ifelse(V(g)$type == TRUE, "lightblue", "salmon")
V(g)$shape = ifelse(V(g)$type == TRUE, "circle", "square")
plot(g, vertex.size=5, vertex.label=NA)

# zwei x 1-Mode Netzwerk aus einem 2-Mode Netzwerk

projections = bipartite_projection(g, which=c("both"), multiplicity = TRUE)
projections
gAus = projections$proj1
gPers = projections$proj2

# die Ausschüsse verbunden über gemeinsame Personen
plot(gAus)
E(gAus)$weight
plot(gAus, edge.width=E(gAus)$weight*3)

# die Personen verbunden über gemeinsame Ausschüsse
plot(gPers, edge.width=E(gPers)$weight*3, vertex.size=5,
     vertex.label.cex=0.6, vertex.label.color="black")

##################################
#Knotengröße nach Degree-Zentralität
deg = centr_degree(gPers)$res
hist(deg)
deg
plot(gPers, edge.width=E(gPers)$weight*1, vertex.size=deg/5,
     vertex.label.cex=deg/70, vertex.label.color="black")

# Und jetzt noch die Parteifarben
hlp = txt
hlp$Ausschuss=NULL
persaus=unique(hlp)
persaus
dim(persaus)
V(gPers)$name
head(persaus)
persaus$farbe="gray"
persaus$farbe[persaus$Partei=="FDP"]="lightblue"
persaus$farbe[persaus$Partei=="AfD"]="darkorange4"
persaus$farbe[persaus$Partei=="SPD"]="plum1"
persaus$farbe[persaus$Partei=="FW"]="orange"
persaus$farbe[persaus$Partei=="GRU"]="palegreen"
head(persaus)

plot(gPers, edge.width=E(gPers)$weight*1, vertex.size=deg/5,
     vertex.label.cex=deg/70, vertex.label.color="black",
     vertex.color=persaus$farbe)
