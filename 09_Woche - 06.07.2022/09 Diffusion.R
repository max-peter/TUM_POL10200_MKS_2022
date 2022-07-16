library(igraph)

#######################
# SI Model 
#######################

# create a random network
g <- sample_smallworld(dim=1, 500, nei=3, p=0.1)
l=layout_with_fr(g)
plot(g,layout=l,vertex.label=NA,vertex.size=3)

# store information about nodes externally
infections <- rep(0,vcount(g))

# now, pick one node randomly
infect <- sample(1:vcount(g),1)
infect
# and "infect" it
infections[infect] <- 1
infections
#
myplot = function(title = "", l = layout_with_fr(g)){
  plot(g, layout=l,
       vertex.color=infections,
       vertex.size=7,vertex.label=NA,
       main=title)
}
myplot("Ausgangszustand",l=l)
infect
neighbors(g,infect)
  
infections[neighbors(g,infect)] <- 1
myplot("Nach 1 Schritt",l=l)

which(infections==1) #number of infected node
infections
sum(infections)

# and now in a script:
infections <- rep(0,vcount(g))
infect <- sample(1:vcount(g),1)
infections[infect] <- 1

# show the plots over time
par(mfrow=c(4,4), mar=c(1,1,1,1))

# let's keep track of the infection process. 
# how many nodes are infected now?
prevalence <- c(sum(infections))
#myplot("1",l)
# repeat the infection process as long as there are uninfected nodes
for (i in 1:16) {
  for (check in which(infections==1)){ # visit all infected nodes
    # and infect their neighbors
    neig=neighbors(g,check)
    
    # infection chance is 30%, otherwise no infection
    p=sample(c(1,2,3,4,5,6,7,8,9,10),length(neig),replace=T)
    neig=neig[p<=3]
    
    infections[neig] <- 1
  }
  # count how many nodes were infected in this step
  prevalence <- c(prevalence,sum(infections))
  #plot every iteration
  myplot(sum(infections),l)
}

dev.off()
# and how did the spreading develop?
prev30=prevalence
plot(prevalence, xlab = "Step", 
     ylab="Number of infected nodes", type = "l")
lines(prev30,col="red")






