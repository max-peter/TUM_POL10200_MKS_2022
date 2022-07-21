aufgabe<-function (g){

    V(g)$type = bipartite.mapping(g)$type
    onemode = bipartite_projection(g, multiplicity = F, which=c("both"))
    gPeople = onemode$proj1
    nPeople = length(gPeople)
    largest <- which.max(components(g)$csize)
    g_comp1 = delete_vertices(gPeople, 
            which(components(gPeople)$membership == largest))
    g_comp2 = delete_vertices(gPeople, 
            which(components(gPeople)$membership != largest))
    t1 = transitivity(g_comp1, type="global")
    t2 = transitivity(g_comp2, type="global")
    d1 = sum(degree(g_comp1))/length(g_comp1)
    d2 = sum(degree(g_comp2))/length(g_comp2)
    c1=closeness(g_comp1)
    c2=closeness(g_comp2)
    ord1=order(c1,decreasing = T)
    ord2=order(c2,decreasing = T)
    name1 = c1[ord1[1:1]]
    name2 = c2[ord2[1:1]]
    name1 = names(name1)
    name2 = names(name2)
    V(g_comp1)$color = ifelse(V(g_comp1)$name != "Julia", "orange", "green")
    V(g_comp1)$shape = ifelse(V(g_comp1)$name != "Julia", "circle", "square")

    V(g_comp2)$color = ifelse(V(g_comp2)$name != "Kerem", "orange", "green")
    V(g_comp2)$shape = ifelse(V(g_comp2)$name != "Kerem", "circle", "square")

 return(list(nPeople,t1,t2,d1,d2,name1,name2,gPeople))
}