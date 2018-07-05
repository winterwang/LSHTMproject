#---------------------------------
# PLOT
#---------------------------------
library("DiagrammeR")
library("V8")
# This code stems from D.L. Dahly 

# build dataframe with predicted class for each observation
x1<-rep(1, nrow(lc1$predclass))        
x2<-lc2$predclass
x3<-lc3$predclass
x4<-lc4$predclass
x5<-lc5$predclass
results <- cbind(x1, x2, x3, x4, x5)
results <-as.data.frame(results)
results

# avoid double naming of classes (because each LCA named their classes 1,2,...,k)
N<-ncol(results) 
n<-0
for(i in 2:N) {
  results[,i]<- (results[,i])+((i-1)+n)
  n<-((i-1)+n)
}

# Make a data frame for the edges and counts
# cross-tabulations and their frequencies
g1<-plyr::count(results,c("x1","x2"))
g2<-plyr::count(results,c("x2","x3"))
colnames(g2)<-c("x1","x2","freq")
g3<-plyr::count(results, c("x3","x4"))
colnames(g3)<-c("x1", "x2","freq")
g4<-plyr::count(results,c("x4","x5"))
colnames(g4)<-c("x1","x2","freq")
edges<-rbind(g1,g2,g3,g4)

# Make a data frame for the class sizes
h1<-plyr::count(results,c("x1"))
h2<-plyr::count(results,c("x2"))
colnames(h2)<-c("x1","freq")
h3<- plyr::count(results,c("x3"))
colnames(h3)<-c("x1","freq")
h4<-plyr::count(results,c("x4"))
colnames(h4)<-c("x1","freq")
h5<-plyr::count(results,c("x5"))
colnames(h5)<-c("x1", "freq")
nodes<-rbind(h1,h2,h3,h4,h5)




#dataframe for nodes - columns: node, label, type, attributes (like color and stuff)
colnames(nodes)<-c("node","label")

#scale nodes
# nodes <- scale_nodes(nodes_df = nodes,
#                      to_scale = nodes$label,
#                      node_attr = "penwidth",
#                      range = c(2, 5))

#dataframe for edges - columns: edge from, edge to, label, relationship, attributes 
colnames(edges)<-c("from", "to", "label")
edges$relationship<-c("given_to")

# #scale edges
# edges <- scale_edges(edges_df = edges,
#                      to_scale = edges$label,
#                      edge_attr = "penwidth",
#                      range = c(1, 5))
# 
# nodes <- scale_nodes(nodes_df = nodes,
#                      to_scale = nodes$penwidth,
#                      node_attr = "alpha:fillcolor",
#                      range = c(5, 90))

nodes
nodes$label2<-nodes$label
nodes$label<-paste0(nodes$node)

# Additional label outside of the ellipses
# nodes$label<-paste0(nodes$node, "',xlabel=","'",nodes$label2) 

# Group-number
#nodes$xlabel<-paste0("(n=",nodes$label2,")")

#plot stuff 
lca_graph<-create_graph(nodes,
                        edges)

render_graph(lca_graph)
