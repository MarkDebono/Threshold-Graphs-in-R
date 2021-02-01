### The function LayerCake plots the Layer-Cake diagram of a threshold graph from its creation sequence ###

LayerCake<-function(Seq){
  if(CompSeq(Seq)$Type==0){CakeSeq<-rep(c(1,0),length.out=length(CompSeq(Seq)$Sequence)-1)}else{CakeSeq<-rep(c(0,1),length.out=length(CompSeq(Seq)$Sequence)-1)}
  
  g<-graph.empty(1,directed="FALSE")
  for (i in 1:length(CakeSeq)){g<-add.vertices(g,1);if(CakeSeq[i]==1){for(i in 1:{vcount(g)-1}){g<-add.edges(g,c(i,vcount(g)))}}}
  n<-vcount(g) #number of sets
  
  if(CompSeq(Seq)$Type==0){
    l<-matrix(c(rep(c(-1,1),length.out=n),floor(seq(0,n,by=0.5))[1:n]),nrow=n,ncol=2,byrow=FALSE)
    if(n>=6)
    {curvededges<-c()
    for (i in seq(2,n-4,2))
    {for(j in seq(4+i,n,2))
    {curvededges<-c(curvededges,which(E(g)==E(g)[i %--% j]))}}
    E(g)$curved<-rep(0,ecount(g))
    E(g)$curved[curvededges]<--0.3}}else{
      l<-matrix(c(rep(c(1,-1),length.out=n),ceiling(seq(0,n,by=0.5))[1:n]),nrow=n,ncol=2,byrow=FALSE)
      if(n>=5)
      {curvededges<-c()
      for (i in seq(1,n-4,2))
      {for(j in seq(4+i,n,2))
      {curvededges<-c(curvededges,which(E(g)==E(g)[i %--% j]))}}
      E(g)$curved<-rep(0,ecount(g))
      E(g)$curved[curvededges]<--0.3}}
  
  plot(g,layout=l,main="Layer-Cake Diagram",vertex.label=CompSeq(Seq)$Sequence)}

### Example 1 ###

LayerCake(c(0,0,0,1,1,1,0,0,0,0,1,1,1,1,0,1))

### Example 2 ###

LayerCake(c(1,0,0,0,1,1,1,0,0,0,0,1,1,1,1,0))