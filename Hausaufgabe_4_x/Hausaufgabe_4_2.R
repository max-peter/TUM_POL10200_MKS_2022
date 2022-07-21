isRunningLocally = Sys.getenv("USERNAME") == "MaxPeter(cironeteu)"
if(isRunningLocally) {
    # set working directory
    # (WD <- getwd())
    # if (!is.null(WD)) setwd(WD)
    # load library
    library(igraph)
    library(igraphdata)
    # load test data
    txt = read.csv("Influencer.txt", sep=" ", encoding = "UTF-8")
    # Den R data.frame in ein Netzwerkobjekt g umwandeln
    g = graph_from_data_frame(txt, directed = F)
}

if(isRunningLocally) {
    # Test the function
    plot (g) # Visualisierung des Netzwerkes
    # Test auf Two-Mode Netzwerk
    bipartite.mapping(g)
    # Anzahl der Komponenten
    components(g)
    comp = components(g)$membership
    comp
    table(comp)
    components(g)$csize
    max(components(g)$csize)
    components(g)$no

    plot(g, vertex.color=comp,vertex.label=NA,vertex.size=10)
    plot(g,vertex.color=V(g)$type,vertex.label=NA)
    plot(g_comp1)
    plot(g_comp2)
    plot(gPeople)

    
    graph.attributes(gPeople)
    length(gPeople)
    plot(gPeople)
    plot(Influencer)
    plot(g_comp2)

}

aufgabe<-function (g){

    V(g)$type = bipartite.mapping(g)$type
    onemode = bipartite_projection(g, multiplicity = F, which=c("both"))
    gPeople = onemode$proj1
    gWeb = onemode$proj2
    nPeople = length(gPeople)
    
    # Transitiviy of comp1 and comp2
    largest <- which.max(components(g)$csize)
    g_comp1 = delete_vertices(gPeople, 
            which(components(gPeople)$membership == largest))
    g_comp2 = delete_vertices(gPeople, 
            which(components(gPeople)$membership != largest))
    t1 = transitivity(g_comp1, type="global")
    t2 = transitivity(g_comp2, type="global")
    # Average degree
    d1 = sum(degree(g_comp1))/length(g_comp1)
    # mean(degree(g_comp1))
    d2 = sum(degree(g_comp2))/length(g_comp2)
    # mean(degree(g_comp2))
    c1=closeness(g_comp1)
    c2=closeness(g_comp2)
    ord1=order(c1,decreasing = T)
    ord2=order(c2,decreasing = T)
    name1 = c1[ord1[1:1]]
    name2 = c2[ord2[1:1]]
    name1 = names(name1)
    name2 = names(name2)

    influencer <- c(name1, name2)
    V(gPeople)$color = ifelse(V(gPeople)$name != influencer, "orange", "green")
    V(gPeople)$shape = ifelse(V(gPeople)$name != influencer, "circle", "square")

  return(list(nPeople,t1,t2,d1,d2,name1,name2,gPeople))

}