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
  #Betweenness Centrality
  bet = betweenness(g, normalized = F)
  ord = order(bet,decreasing = T)
  chefs = bet[ord[1:3]]
  return(chefs)
}

aufgabe_b<-function (g){
  #Eigenvector Centrality
  eig = eigen_centrality(g)
  eig = eig$vector
  ord = order(eig,decreasing = F)
  mentees = eig[ord[1:3]]
  return(mentees)
}

aufgabe_c<-function (g){
  #Degree Centrality
  deg = degree(g, normalized = F)
  ord = order(deg,decreasing = T)
  kommunikationstalent = deg[ord[1:1]]
  return(kommunikationstalent)
}