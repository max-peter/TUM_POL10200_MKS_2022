aufgabe_a <- function(g){

  g1 = delete_vertices(g,
            which(components(g)$membership != 1))
  g2 = delete_vertices(g,
            which(components(g)$membership != 2))
  return(list(g1,g2))
}
aufgabe_b <- function(g){

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

  return(g)
}
makamer=c(9, 17)
aufgabe_d <- function(g) {
  
  # Generate colors based on kooperation:
  colrs <- c("green", "red")
  colrs[V(g)$kooperation]
  V(g)$color <- colrs[V(g)$kooperation]
  
  # Set vertex size based on haarlaenge:
  V(g)$size <- V(g)$haarlaenge*0.5
  
  # set label to namen
  V(g)$label <- V(g)$namen

  # label size to haarlaenge
  V(g)$label.cex <- V(g)$haarlaenge*0.015

  return(g)
}
# aufgabe_e
# Namen der beiden Bewohner in alphabetischer Reihenfolge in der Variable sulaner
sulaner=c("Kelii", "Malio" )