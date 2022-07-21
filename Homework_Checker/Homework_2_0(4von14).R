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