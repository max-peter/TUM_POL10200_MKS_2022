library (igraph)
# Einlesen der Textdatei in eine Variable (R data.frame)
txt = read.csv("simple.csv", sep=",")
head(txt)
dim(txt) # wie viele Zeilen und Spalten hat mein data.frame?
# Den R data.frame in ein Netzwerkobjekt umwandeln
g = graph_from_data_frame(txt, directed = F)
plot (g) # Visualisierung des Netzwerkes

# Eigenschaften vom Netzwerkobjekt
g
E(g)[1] # die erste Kante
ecount(g) # Anzahl der Kanten
vcount(g) # Anzahl der Knoten
V(g) # die Liste der Knoten
vertex_attr(g) # Knotenattribute
edge_attr(g) # Kantenattribute

# Das Netzwerk als Matrix
g[]
g[1,3] = 1 # Zuweisen eines Kantenwertes
g[]
plot(g)
g[1,3] = 0 # L�schen einer Kante
plot(g)

# Vereinfachen des Netzwerkes, also Entfernen von doppelten
# Kanten und Self-Loops
g = simplify(g, remove.multiple = T, remove.loops = T)
plot(g)
ecount(g)

#######################################

# Laden der neuen Datei, Achtung: Den richtigen Spaltenteiler 
# festlegen (sep). Backslash \ am Mac = alt-shift-7
txt = read.csv("ausschuesse.txt", sep="\t", encoding = "UTF-8")
head(txt)
dim(txt)
g = graph_from_data_frame(txt, directed = F)
plot(g)
ecount(g)
vcount(g)
plot(g, vertex.label=NA)
plot(g, vertex.size=5, vertex.label=NA)
g = simplify(g, remove.multiple = T, remove.loops = T)
plot(g, vertex.size=5, vertex.label=NA)

# Wir erzeugen einen Vektor zum Einf�rben der Knoten nach
# Knotentypen 
V(g)$name
col1 = rep("orange", 14) # es gibt 14 Aussch�sse
col1
col2 = rep("lightblue", vcount(g) - 14) # der Rest sind Personen
col2
length(col2)
col=c(col1,col2)
length(col)
# Dies wird jetzt zum F�rben der Knoten verwendet
# Zudem werden weitere optische Ver�nderungen gemacht
plot(g, vertex.size=5, vertex.label=NA, vertex.color=col)
# und jetzt noch die Labels in schwarz und ein bisschen kleiner
plot(g, vertex.size=7, vertex.label.cex=0.6, vertex.color=col,
     vertex.label.color="black")

#edge.color="black"

V(g)$name
