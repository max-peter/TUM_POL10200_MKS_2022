isRunningLocally = Sys.getenv("USERNAME") == "MaxPeter(cironeteu)"
if(isRunningLocally) {
    # set working directory
    
    # load library
    library(igraph)
    library(igraphdata)
    # load test data
    txt = read.csv("Firma.txt", sep=";", encoding = "UTF-8")
    # Den R data.frame in ein Netzwerkobjekt g umwandeln
    g = graph_from_data_frame(txt, directed = F)
}

if(isRunningLocally) {
    # Test the function
    plot (g) # Visualisierung des Netzwerkes
}

aufgabe_a<-function (g){
  g = simplify(g, remove.multiple = T, remove.loops = T)
  #Anzahl der Knoten und Kanten
  knoten = vcount(g)
  kanten = ecount(g)
  return(c(knoten, kanten))
}

aufgabe_b<-function (g){
  #Netzwerklösung 02UE
  bipartite.mapping(g)
  V(g)$type = bipartite.mapping(g)$type
  #Zuordnung shape und color
  V(g)$color = ifelse(V(g)$type == TRUE, "blue", "red")
  V(g)$shape = ifelse(V(g)$type == TRUE, "square", "circle")
  return(g)
}

aufgabe_c<-function (g){
  #1. Degree Centrality
  deg = degree(g, normalized = T)
  ord = order(deg,decreasing = T)
  meiste = deg[ord[1:4]]
  return(meiste)
}

aufgabe_d<-function (g){
  projections = bipartite_projection(g, which=c("both"), multiplicity = TRUE)
  gM = projections$proj1
  gC = projections$proj2
  manager = vcount(gM)
  companies = vcount(gC)
  return(c(manager, companies))
}