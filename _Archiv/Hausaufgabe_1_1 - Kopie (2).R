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
  g = simplify(g, remove.multiple = T, remove.loops = T)
  manager = length(unique(txt$Manager))
  companies = length(unique(txt$Company))
  return(c(manager, companies))
}