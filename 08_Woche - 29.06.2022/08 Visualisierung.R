##    Based on:
##    Katya Ognyanova, katya@ognyanova.net              ##
##    www.kateto.net/netscix2016                        ##
##                                                      ##
##======================================================##

#Visualisierungsattribute
###NODES	 
#vertex.color	 Node color
#vertex.frame.color	 Node border color
#vertex.shape	 One of "none", "circle", "square", "csquare", "rectangle"
#"crectangle", "vrectangle", "pie", "raster", or "sphere"
#vertex.size	 Size of the node (default is 15)
#vertex.size2	 The second size of the node (e.g. for a rectangle)
#vertex.label	 Character vector used to label the nodes
#vertex.label.family	 Font family of the label (e.g."Times", "Helvetica")
#vertex.label.font	 Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
#vertex.label.cex	 Font size (multiplication factor, device-dependent)
#vertex.label.dist	 Distance between the label and the vertex
#vertex.label.degree	 The position of the label in relation to the vertex,
#where 0 right, "pi" is left, "pi/2" is below, and "-pi/2" is above
###EDGES	 
#edge.color	 Edge color
#edge.width	 Edge width, defaults to 1
#edge.arrow.size	 Arrow size, defaults to 1
#edge.arrow.width	 Arrow width, defaults to 1
#edge.lty	 Line type, could be 0 or "blank", 1 or "solid", 2 or "dashed",
#3 or "dotted", 4 or "dotdash", 5 or "longdash", 6 or "twodash"
#edge.label	 Character vector used to label edges
#edge.label.family	 Font family of the label (e.g."Times", "Helvetica")
#edge.label.font	 Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
#edge.label.cex	 Font size for edge labels
#edge.curved	 Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
#arrow.mode	 Vector specifying whether edges should have arrows,
#possible values: 0 no arrow, 1 back, 2 forward, 3 both
###OTHER	 
#margin	 Empty space margins around the plot, vector with length 4
#frame	 if TRUE, the plot will be framed
#main	 If set, adds a title to the plot
#sub	 If set, adds a subtitle to the plot

library(igraph)
load(url("http://www.pfeffer.at/r/visnet.rrr")) 
plot(net)

vertex_attr(net)

plot(net, edge.arrow.size=0.2, edge.curved=0,
     vertex.color="orange", vertex.frame.color="black",
     vertex.label=V(net)$media, vertex.label.color="black",
     vertex.label.cex=.7) 

# The second way to set attributes is to add them to the igraph object.
V(net)$media.type

# Generate colors based on media type:
colrs <- c("gray50", "tomato", "gold")
colrs[V(net)$media.type]
V(net)$color <- colrs[V(net)$media.type]

plot(net)

# Set node size based on audience size:
V(net)$size <- V(net)$audience.size*0.7

# The labels are currently node IDs.
# Setting them to NA will render no labels:
V(net)$label <- NA

# Set edge width based on weight:
edge_attr(net)
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .4
E(net)$edge.color <- "gray80"

plot(net) 

# we can save and load everything!
save(file="net.rrr",net)
rm(net)
load("net.rrr")
plot(net)

# We can also override the attributes explicitly in the plot:
plot(net, edge.color="orange", vertex.color="gray50") 


# We can also add a legend explaining the meaning of the colors we used:
plot(net) 
legend(x=-1.1, y=-1.1, c("Newspaper","Television", "Online News"),
       pch=21,pt.bg=colrs, pt.cex=1.8, bty="n")


# Sometimes, especially with semantic networks, we may be interested in 
# plotting only the labels of the nodes:

plot(net, vertex.shape="none", vertex.label=V(net)$media, 
     vertex.label.font=2, vertex.label.color="gray40",
     vertex.label.cex=.5, edge.color="gray85")


# Let's color the edges of the graph based on their source node color.
# We'll get the starting node for each edge with "ends()".
E(net)
ends(net, es=E(net), names=F)[,1]
edge.start <- ends(net, es=E(net), names=F)[,1]
edge.col <- V(net)$color[edge.start]

plot(net, edge.color=edge.col, edge.curved=.1)


#  ------->> Network Layouts --------

plot(net)
# Network layouts are algorithms that return coordinates for each
# node in a network.

# You can set the layout in the plot function:
plot(net, layout=layout_randomly)

# Or calculate the vertex coordinates in advance:
l <- layout_in_circle(net)
head(l)
plot(net, layout=l)

# l is simply a matrix of x,y coordinates (N x 2) for the N nodes in the graph. 
# You can generate your own:
l <- cbind(1:vcount(net), c(1, vcount(net):2))
plot(net, layout=l)

# This layout is just an example and not very helpful - thankfully
# igraph has a number of built-in layouts, including:

# Circle layout
l <- layout_in_circle(net)
plot(net, layout=l)

# The Fruchterman-Reingold force-directed algorithm 
# Nice but slow, most often used in graphs smaller than ~1000 vertices. 
l <- layout_with_fr(net)
plot(net, layout=l)

# You will also notice that the layout is not deterministic - different runs 
# will result in slightly different configurations. Saving the layout in l
# allows us to get the exact same result multiple times.
par(mfrow=c(2,2), mar=c(1,1,1,1))
plot(net, layout=layout_with_fr)
plot(net, layout=layout_with_fr)
plot(net, layout=layout_with_fr)
plot(net, layout=layout_with_fr)

dev.off()


# Another popular force-directed algorithm that produces nice results for
# connected graphs is Kamada Kawai. Like Fruchterman Reingold, it attempts to 
# minimize the energy in a spring system.

l <- layout_with_kk(net)
plot(net, layout=l)

# The LGL algorithm is for large connected graphs. Here you can specify a root - 
# the node that will be placed in the middle of the layout.
plot(net, layout=layout_with_lgl)

# By default, igraph uses a layout called layout_nicely which selects
# an appropriate layout algorithm based on the properties of the graph. 
plot(net)

# 3D sphere layout
l <- layout_on_sphere(net)
plot(net, layout=l)
head(l)


#  ------->> R plots and colors --------

# If you plan on using the built-in color names, here's what they are: 
colors()

# We can also set the opacity/transparency using the parameter 'alpha' (range 0-1):
plot(x=1:5, y=rep(5,5), pch=19, cex=16, col=rgb(.25, .5, .3, alpha=.5), xlim=c(0,6))  

# In many cases, we need a number of contrasting colors, or multiple shades of a color.
# R comes with some predefined palette function that can generate those for us.
pal1 <- heat.colors(5, alpha=1)   # generate 5 colors from the heat palette, opaque
pal2 <- rainbow(5, alpha=.5)      # generate 5 colors from the heat palette, semi-transparent
plot(x=1:10, y=1:10, pch=19, cex=10, col=pal1)
plot(x=10:1, y=1:10, pch=19, cex=10, col=pal2)



# Check out all available layouts in igraph:
?igraph::layout_

layouts=c("layout_in_circle","layout_nicely","layout_on_grid",
  "layout_randomly","layout_with_dh","layout_with_drl",
  "layout_with_fr","layout_with_gem","layout_with_graphopt",
  "layout_with_kk","layout_with_lgl","layout_with_mds")  

par(mfrow=c(3,4), mar=c(1,1,1,1))

for (layout in layouts) {
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, layout=l, main=layout) }

dev.off()

