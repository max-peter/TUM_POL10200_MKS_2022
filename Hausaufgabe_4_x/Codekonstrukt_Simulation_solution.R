library(igraph)

# g=sample_smallworld(dim=1, luecke_a, nei=3, p=0.4)
luecke_a = 1001
# infect = sample(1:vcount(g),luecke_b)
luecke_b = 1
# infections[infect] = luecke_c
luecke_c = 1
# tage[infect]
luecke_d = 3
# for (i in 1:luecke_e) 
luecke_e = 30
# for (knoten in which(infections==luecke_f))
luecke_f = 1
# if (tage[knoten]==luecke_g){
luecke_g = 7
# infections[knoten]=luecke_h
luecke_h = 2
luecke_i = 55
luecke_j = 1
luecke_k = 2
luecke_l = 14


#### Ausgangssituation ######
#infections = Status des Knotens 
#(0=anfaellig, 1=infiziert, 2=immun)
#tage = Tage seit Ansteckung des Knotens
g=sample_smallworld(dim=1, luecke_a, nei=3, p=0.4)
infections = rep(0,vcount(g))
tage = rep(0,vcount(g))

#Patient Null infizieren
infect = sample(1:vcount(g),luecke_b)
infections[infect] = luecke_c
tage[infect] = luecke_d

##### Simulation ######
par(mfrow=c(4,4), mar=c(1,1,1,1))

for (i in 1:luecke_e) {
  print(i)
  #Besuche alle infizierten Knoten
  for (knoten in which(infections==luecke_f)){ 
  
    #Erhoehe die Anzahl an Tagen seit Ansteckung
    tage[knoten]=tage[knoten]+1
    print(tage[knoten])
    
    #Immun werden (nach .. Tagen)
    if (tage[knoten]==luecke_g){
      infections[knoten]=luecke_h
    }
    #oder Nachbarn infizieren
    else {
      #Speichere alle Nachbarn
      neig=neighbors(g,knoten)
      #Waehle die nicht-immunen Nachbarn aus
      neig=neig[infections[neig]==0] 
      #Stecke die nicht-immunen Nachbarn mit einer gewissen
      #Wahrscheinlichkeit an
      p=sample(1:100,length(neig),replace=T)
      neig=neig[p<=luecke_i]
      infections[neig] = luecke_j
    }
  }
  #Besuche alle immunen Knoten
  for (knoten in which(infections==luecke_k)){ 
    #Erhoehe die Anzahl an Tagen seit Ansteckung
    tage[knoten]=tage[knoten]+1
    
    #Immunisierung verlieren (falls .. Tage vergangen sind)
    if (tage[knoten]==luecke_l){
      infections[knoten]=0 #Infizierung zuruecksetzen
      tage[knoten]=0 #Tage seit Ansteckung zuruecksetzen
    }
  }
  #Plotte das Netzwerk
  plot(g, vertex.color=infections, vertex.label=NA)
}

