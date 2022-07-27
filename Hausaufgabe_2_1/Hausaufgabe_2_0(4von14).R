isRunningLocally = Sys.getenv("USERNAME") == "MaxPeter(cironeteu)"
if(isRunningLocally) {
    # load library
    library(igraph)
    library(igraphdata)
    # Load rstudioapi package
    library("rstudioapi")
    
    # set working directory
    # (WD <- getwd())
    # if (!is.null(WD)) setwd(WD)
    # Set working directory to source file location
    # https://statisticsglobe.com/set-working-directory-to-source-file-location-automatically-in-rstudio
    setwd(dirname(getActiveDocumentContext()$path))
    # Check updated working directory
    getwd()
    
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

  dg1 = degree_distribution(g1, cumulative = TRUE)
  dg2 = degree_distribution(g2, cumulative = TRUE)
  dg3 = degree_distribution(g3, cumulative = TRUE)
  dg4 = degree_distribution(g4, cumulative = TRUE)

  return(list(dg1, dg2, dg3, dg4))

}
homophily <- function (g) {

  edges = get.data.frame(g)
  internal=length(which(edges$from == edges$to))
  external=length(which(edges$from != edges$to))
  internalprop = internal / (internal + external)
  col=ifelse(edges$from == edges$to, "red", "blue")

  return(internalprop)

}
aufgabe_c <- function(g1, g2, g3, g4) {

  V(g1)$geschlecht = c(rep(1, 15), rep(2, 15))
  V(g2)$geschlecht = c(rep(1, 15), rep(2, 15))
  V(g3)$geschlecht = c(rep(1, 15), rep(2, 15))
  V(g4)$geschlecht = c(rep(1, 15), rep(2, 15))

  hg1=homophily(g1)
  hg2=homophily(g2)
  hg3=homophily(g3)
  hg4=homophily(g4)
  return(c(hg1, hg2, hg3, hg4))

}
echtes_netzwerk <- "g1"
erdos_renyi <- "g2"
small_world <- "g3"
barabasi_albert <- "g4"