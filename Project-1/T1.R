library(igraph)
a=list.files("C:/Users/evash/Documents/GWU/17-Fall-GWU/Big Data/project1/Edges")
dir=paste("C:/Users/evash/Documents/GWU/17-Fall-GWU/Big Data/project1/Edges/", a, sep = "")
n=length(dir)
merge.data = read.csv(file=dir[3], header = F, sep = "")
for (j in 2:n) {
  new.data = read.csv(file = dir[j], header = F, sep = "")
  merge.data = rbind(merge.data, new.data)
}
graph<- graph_from_data_frame(merge.data, directed = TRUE, vertices = NULL)
#write(degree(graph), file = "C:/Users/evash/Documents/GWU/17-Fall-GWU/Big Data/project1/graph_degree.txt")
is.simple(graph)
sgraph<- simplify(graph,remove.multiple = TRUE, remove.loops = TRUE)
is.simple(sgraph)
diameter(sgraph,unconnected=FALSE)
get.diameter(sgraph)
degree(sgraph, mode = "all")
sgraph_degree <- degree(sgraph, mode="all")
c<- V(sgraph)[sgraph_degree < 200]
sgraph_new <- delete.vertices(sgraph, c)
new_degree <- degree(sgraph_new, mode = "all")
c<- V(sgraph_new)[new_degree = 0]
new <- delete.vertices(sgraph_new, c)
E(new)$width <- as.numeric(1.0)
plot(new,vertex.size=8, vertex.color="red",edge.arrow.size=0.5,vertex.label=NA)
#plot(sgraph_new)
degree(sgraph_new, mode = "all")
graph_closeness = sort(closeness(sgraph_new), decreasing = TRUE)
head(graph_closeness, 10)
#largest_cliques(sgraph_new)
#centr_betw(sgraph_new)
#power_centrality(sgraph_new)
diameter(sgraph_new,unconnected=FALSE)
#node.diameter <- get.diameter(sgraph_new)
get.diameter(sgraph_new)
#plot(sgraph_new,layout=layout.fruchterman.reingold,vertex.size=6, vertex.color="yellow",vertex.label=NA)
E(sgraph_new)$width <- as.numeric(1.0)
plot(sgraph_new,vertex.size=8, vertex.color="red",edge.arrow.size=0.5,vertex.label=NA)

V(sgraph_new)$color<-"skyblue"
V(sgraph_new)$size<-as.numeric(2)
V(sgraph_new)[node.diameter]$color<-"darkgreen"
V(sgraph_new)[node.diameter]$size<-as.numeric(10)
V(sgraph_new)[node.diameter]$label.color<-"white"
E(sgraph_new)$color<-"grey"
# all non-diameter edges will be grey
#E(sgraph_new,path=node.diameter)$color<-"darkgreen"
#E(sgraph_new,path=node.diameter)$width<-as.numeric(2)
par(mar=c(.1,.1,.1,.1))
plot(sgraph_new)

g <- make_ring(14)
ego_size(g, 0, 1:3)
ego_size(g, 1, 1:3)
ego_size(g, 2, 1:3)
ego(g, 0, 1:3)
ego(g, 1, 1:3)
ego(g, 2, 1:3)

# attributes are preserved
V(g)$name <- c(813491,50393960,174853,229523,13535762,793219,30313925,5676102,113963,11336782,15948437,586,2384071,3839)
make_ego_graph(g, 2, 1:3)

# connecting to the neighborhood
#g <- make_ring(10)
#g <- connect(g, 2)
plot(g)


