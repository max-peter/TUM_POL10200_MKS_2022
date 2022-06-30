isRunningLocally = Sys.getenv("USERNAME") == "MaxPeter(cironeteu)"
if(isRunningLocally) {
    # set working directory
    
    # load library
    library(igraph)
    library(igraphdata)
    # load test data
    txt = read.csv("Firma.txt", sep=";", encoding = "UTF-8")
    # Den R data.frame in ein Netzwerkobjekt g umwandeln
    g = graph_from_data_frame(txt, directed = F)
}

if(isRunningLocally) {
    # Test the function
    plot (g) # Visualisierung des Netzwerkes
}
