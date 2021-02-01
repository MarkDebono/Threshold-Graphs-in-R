### The function is.threshold determines whether or not a graph is a threshold graph ###

is.threshold<-function(g){
  degseq<-degree(g,V(g))
  degseq<-sort(degseq,decreasing=TRUE) #sorting the degree sequence in non-increasing form
  
  repeat{
    degseq<-degseq[degseq!=0];
    if(length(degseq)==0){print("Graph is threshold");break};
    if(degseq[1]==length(degseq)-1){degseq<-degseq[-1];degseq<-degseq-1}else{print("Graph is not threshold");break}}}

### Example 1 ###

A<-matrix(c(0,1,1,1,1,0,0,0,1,0,0,1,1,0,1,0),nrow = 4,ncol=4,byrow=TRUE)
g<-graph.adjacency(A,"undirected")
plot(g)
is.threshold(g)

### Example 2 ###

A<-matrix(c(0,1,1,1,0,1,0,1,1,0,1,1,0,0,1,1,1,0,0,1,0,0,1,1,0),nrow = 5,ncol=5,byrow=TRUE)
g<-graph.adjacency(A,"undirected")
plot(g)
is.threshold(g)
