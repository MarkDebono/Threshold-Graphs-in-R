### The function ThresholdGraph draws a threshold graph from its creation sequence using the layout of the layer-cake diagram###

ThresholdGraph<-function(Seq){
  n<-length(CompSeq(Seq)$Sequence)
  
  if(CompSeq(Seq)$Type==0){
    l<-matrix(c(rep(c(-1,1),length.out=n),floor(seq(0,n,by=0.5))[1:n]),nrow=n,ncol=2,byrow=FALSE)}else{
      l<-matrix(c(rep(c(1,-1),length.out=n),ceiling(seq(0,n,by=0.5))[1:n]),nrow=n,ncol=2,byrow=FALSE)}
  
  l2<-c()
  for(i in 1:length(CompSeq(Seq)$Sequence))
  {if(CompSeq(Seq)$Sequence[i]==1){l2<-rbind(l2,l[i,])}else{rand<-runif(1,0,2*pi);
  l2<-rbind(l2,cbind(l[i,1]+0.1*cos(rand+2*pi/CompSeq(Seq)$Sequence[i]*seq(0,CompSeq(Seq)$Sequence[i]-1)),l[i,2]+0.1*sin(rand+2*pi/CompSeq(Seq)$Sequence[i]*seq(0,CompSeq(Seq)$Sequence[i]-1))))}
  }
  
  g<-graph.empty(1,directed="FALSE")
  for (i in 1:length(Seq)){g<-add.vertices(g,1);if(Seq[i]==1){for(i in 1:{vcount(g)-1}){g<-add.edges(g,c(i,vcount(g)))}}}
  plot(g,layout=l2,vertex.label=NA,vertex.size=6)}

### Example 1 ###

ThresholdGraph(c(0,0,0,1,1,1,0,0,0,0,1,1,1,1,0,1))

### Example 2 ###

ThresholdGraph(c(1,0,0,0,1,1,1,0,0,0,0,1,1,1,1,0))

