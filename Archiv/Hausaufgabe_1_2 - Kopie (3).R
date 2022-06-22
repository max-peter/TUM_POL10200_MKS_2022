aufgabe_a<-function (g){
  #Betweenness Centrality
  bet = betweenness(g, normalized = F)
  ord = order(bet,decreasing = T)
  chefs = bet[ord[1:3]]
  return(chefs)
}

aufgabe_b<-function (g){
  #closeness Centrality
  cc=closeness(g, normalized = F)
  ord = order(cc,decreasing = T)
  mentees = cc[ord[1:3]]
  return(mentees)
}

aufgabe_c<-function (g){
  #Degree Centrality
  deg = degree(g, normalized = F)
  ord = order(deg,decreasing = T)
  kommunikationstalent = deg[ord[1:1]]
  return(kommunikationstalent)
}