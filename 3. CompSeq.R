### The function CompSeq outputs the compact creation sequence from the creation sequence ###

CompSeq<-function(Seq)
{CompSeq<-c()
Counter<-1
CompSeq[Counter]<-2
Type<-Seq[1] #type of the 1st group
for(i in 2:length(Seq))
{if(Seq[i]==Seq[i-1]){CompSeq[Counter]<-CompSeq[Counter]+1}else{Counter<-Counter+1;CompSeq[Counter]=1}};
return(list(Sequence=CompSeq,Type=Type))}

### Example 1 ###

CompSeq(c(0,0,0,1,1,1,0,0,0,0,1,1,1,1,0,1))

### Example 2 ###

CompSeq(c(1,0,0,0,1,1,1,1,0,0,0,1,1,1,0))