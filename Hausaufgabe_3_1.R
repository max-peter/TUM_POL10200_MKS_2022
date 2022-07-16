isRunningLocally = Sys.getenv("USERNAME") == "MaxPeter(cironeteu)"
if(isRunningLocally) {
    # set working directory
    # load library
    library(igraph)
    library(igraphdata)
    # load test data
    load("inselpolitik.rrr")
    plot(g)
}
if (isRunningLocally) {
  # Visualisierung des Netzwerkes
  # g1 mit den neuen Attributen plotten
  comp=components(g)$membership
  plot(g, vertex.color=comp,vertex.label=comp,vertex.size=10)
  plot(g1)
  plot(g2)
}
if (isRunningLocally) {
    # Visualisierung des Netzwerkes
    # g1 mit den neuen Attributen plotten
    plot(aufgabe_b(aufgabe_a(g)[[1]]))
}

aufgabe_a <- function(g){

  g1 = delete_vertices(g,
            which(components(g)$membership != 1))
  g2 = delete_vertices(g,
            which(components(g)$membership != 2))
  return(list(g1,g2))
}

aufgabe_b <- function(g){

  # Check attributes
  g = g1
  vertex_attr(g)
  edge_attr(g)
  
  # Generate colors based on kooperation:
  colrs <- c("green", "red")
  colrs[V(g)$kooperation]
  V(g)$color <- colrs[V(g)$kooperation]
  
  # Set edge size based on trust:
  E(g)$width <- E(g)$trust*0.5
  
  #change edge color: RGB-Wert (89,70,94)
  farbe=rgb(89/255,70/255,94/255)
  E(g)$color <- farbe

  #change vertex shape based on communities
  communities <- cluster_edge_betweenness(g)
  shape = c("circle", "square", "sphere")
  V(g)$shape <- shape[membership(communities)]

  plot(g)
  return(g)
}

# Aufgabe_c
# Nummern der beiden Knoten in aufsteigender Reihenfolge in der Variable makamer
makamer=c(9, 17)

# Check attributes Insel Sulani
  g = g2
  vertex_attr(g)
  edge_attr(g)
  plot (g)

aufgabe_d <- function(g){
  
  # Generate colors based on kooperation:
  colrs <- c("green", "red")
  colrs[V(g)$kooperation]
  V(g)$color <- colrs[V(g)$kooperation]
  
  # Set vertex size based on haarlaenge:
  V(g)$size <- V(g)$haarlaenge*0.5
  
  # set label to namen
  V(g)$label <- V(g)$namen

  # label size to 
  V(g)$label.cex <- V(g)$haarlaenge*0.015

  return(g)
}
# aufgabe_e
# Namen der beiden Bewohner in alphabetischer Reihenfolge in der Variable sulaner
sulaner=c("Kelii", "Malio" )