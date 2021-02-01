### The function GraphFromSeq plots a threshold graph from its creation sequence ###

GraphFromSeq<-function(Seq){
  g<-graph.empty(1,directed="FALSE");
  for (i in 1:length(Seq)){
    g<-add.vertices(g,1);
    if(Seq[i]==1){for(i in 1:{vcount(g)-1}){g<-add.edges(g,c(i,vcount(g)))}}};
  plot(g)}

### Example ###

GraphFromSeq(c(1,0,1,1))