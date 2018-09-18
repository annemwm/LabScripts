library("igraph")
library("splitstackshape")

setwd("/Users/dh_lab_03/Desktop/all_edges_nodes/marvel_edges")
marvel_edges <- data.frame()
tmp = list.files(pattern="*.csv")
marvel_df = lapply(tmp, read.csv)
for (df in marvel_df){
  marvel_edges<-rbind(marvel_edges, df)
}

setwd("/Users/dh_lab_03/Desktop/all_edges_nodes/marvel")
marvel_chars <- data.frame()
tmp = list.files(pattern="*.csv")
marvel_df = lapply(tmp, read.csv)
for (df in marvel_df){
  marvel_chars<-rbind(marvel_chars, df)
}
marvel_nodes <- data.frame(marvel_chars$CHARACTER)


g <- graph.data.frame(marvel_edges, directed = FALSE, vertices = NULL)
g2 <- graph.adjacency(get.adjacency(g), mode=c("undirected"), weighted=TRUE)

length(V(g))
length(V(g2))

length(E(g))
length(E(g2))

sort(degree(g), decreasing = T)
sort(degree(g2), decreasing = T)

evc <- evcent(g, directed=FALSE)
sort(evc$vector, TRUE)
sort(evcent(g2)$vector, TRUE)[1:10]


sort(closeness(g, vids=V(g), mode = c("all"), weights= NULL, normalized = FALSE), TRUE)[1:10]

graph.strength(g2, vids=V(g2), mode = c("all"))
sort(graph.strength(g2, vids=V(g2), mode = c("all")), TRUE)[1:10]


diameter(g2, directed = FALSE, weights= E(g2)$weight)
farthest.nodes(g2, weights=E(g2)$weight)

#percentage of length 2 paths: how many closed loops there are 
transitivity(g, type=c("undirected"), vids=NULL, weights=NULL, isolates=c("NaN"))

# how interconnected graph is out of all possible connects
edge_density(g)

# assortativity: how often nodes from same group are connected. will have to come back once gender is tagged...


robustness_vector<-vector()
for (j in 1:1000){
  random.nodes<-sample(V(g2), length(V(g2)), replace = FALSE, prob= NULL)
  g.robust<-as.undirected(g2)
  for (i in 1:length(V(g2))) {
    g.robust<-delete.vertices(g.robust, v=as_ids(random.nodes[[i]]))
    if (is.connected(g.robust)==FALSE)
      break
  }
  robustness_vector<-append(robustness_vector, i/length(V(g2)))
}

# vulnerability
deg<-names(sort(degree(g2), TRUE))
g.vulnerable<-as.undirected(g2)
for (i in length(V(g2))){
  g.vulnerable<-delete.vertices(g.vulnerable, v=deg[[i]])
  if (is.connected(g.vulnerable)==FALSE)
    break
}
vuln <- i
print(vuln)
vuln/length(V(g2))

plot.igraph(g, layout=layout_with_fr, edge.width=E(g)$weight, vertex.size=degree(g)/10,
            vertex.color="white",vertex.label.color="black", vertex.frame.color="white")
