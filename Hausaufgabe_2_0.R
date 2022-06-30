isRunningLocally = Sys.getenv("USERNAME") == "MaxPeter(cironeteu)"
if(isRunningLocally) {
    # set working directory
    # load library
    library(igraph)
    library(igraphdata)
    # load test data
    g1 = read_graph("g1.txt", format =  "edgelist", directed=F)
    g2 = read_graph("g2.txt", format =  "edgelist", directed=F)
    g3 = read_graph("g3.txt", format =  "edgelist", directed=F)
    g4 = read_graph("g4.txt", format =  "edgelist", directed=F)
    
}

if(isRunningLocally) {
    # Visualisierung des Netzwerkes
    plot(g1)
    plot(g2)
    plot(g3)
    plot(g4)
}
aufgabe_a <- function(g1, g2, g3, g4) {

  tg1 = transitivity(g1)
  tg2 = transitivity(g2)
  tg3 = transitivity(g3)
  tg4 = transitivity(g4)

  return(c(tg1, tg2, tg3, tg4))

}
aufgabe_b <- function(g1, g2, g3, g4) {

  dg1 = degree(g1, type = "global")
  dg2 = degree(g2, type = "global")
  dg3 = degree(g3, type = "global")
  dg4 = degree(g4, type = "global")

  return(list(dg1, dg2, dg3, dg4))

}
homophily <- function (g) {

  #Hier kommen Ihre Befehle
  #....

  return (internalprop)

}
aufgabe_c <- function(g1, g2, g3, g4) {

  #Hier kommen Ihre Befehle
  #....

  return(c(hg1, hg2, hg3, hg4))

}
echtes_netzwerk <- "g1"
erdos_renyi <- "g2"
small_world <- "g3"
barabasi_albert <- "g4"