library(igraph)

myplot = function(title = "", l = layout_with_fr(g)){
  plot(g, layout=l,
       vertex.color=infections,
       vertex.size=7,vertex.label=NA,
       main=title)
}

si_model <- function(g, infectprob = 3){
  # and now in a script:
  infections <<- rep(0,vcount(g)) # <<- is neccessary here for myplot to "see" the list
  infect <- sample(1:vcount(g),1)
  infections[infect] <<- 1

  # show the plots over time
  par(mfrow=c(4,4), mar=c(1,1,1,1))

  # let's keep track of the infection process. 
  # how many nodes are infected now?
  prevalence <<- c(sum(infections))
  myplot("1",l)
  # repeat the infection process as long as there are uninfected nodes
  while (any(infections==0)) {
    for (check in which(infections==1)){ # visit all infected nodes
      # and infect their neighbors
      neig=neighbors(g,check)
      
      # infection chance is infectprob%, otherwise no infection
      p=sample(c(1,2,3,4,5,6,7,8,9,10),length(neig),replace=T)
      neig=neig[p<=infectprob]
      
      infections[neig] <<- 1
    }
    # count how many nodes were infected in this step
    prevalence <<- c(prevalence,sum(infections))
    #plot every iteration
    myplot(sum(infections),l)
  }

  dev.off()
  # and how did the spreading develop?
  plot(prevalence, xlab = "Step", 
       ylab="Number of infected nodes", type = "l",
       main=paste0("Infections for ",infectprob*10,"% infection probability"))
}

g <- sample_gnm(200, 2500, directed = T)
diameter(g)
l=layout_with_fr(g)
components(g)$no
si_model(g, infectprob = 5)

g <- sample_pa(500, m=2, directed=F)
l=layout_with_fr(g)
si_model(g, infectprob = 7)

g <- sample_smallworld(dim=1, 500, nei=3, p=0.1)
l=layout_with_fr(g)
diameter(g)
si_model(g, infectprob = 2)
