N=1000
model_contamination<- function(H,alpha,beta,gamma){
  X<-NULL
  X[1]<-1
  for (i in 1:H){
    proba<-c(alpha/(alpha+beta+X[i]*gamma),beta/(alpha+beta+X[i]*gamma),(X[i]*gamma)/(alpha+beta+X[i]*gamma))
    U<-sample(c(-1,0,1),H,replace=TRUE, prob=proba)
    X[i+1]<-(X[i]+U[i])*(X[i]>0)+ (X[i]==0)*(U[i]==1)
  }
  simu<-data.frame(0:1000,X)
  colnames(simu)<-c("n","X_n")
  return(simu)
}

simu_conta1<-model_contamination(N,5,7,8)
ggplot(data=simu_conta1,aes(n,X_n)) + geom_line(color="#E69F00")+geom_point(color="#E69F00")+ylab(expression(X[n]))+ ggtitle(expression(paste("Représentation du processus")))+
annotate("text", x=100, y =1.15, label = expression(paste(alpha,"=5  ", beta,"=7  ",gamma,"=15    ")))

simu_conta2<-model_contamination(N,5,7,15)
ggplot(data=simu_conta2,aes(n,X_n)) + geom_line(color="#E69F00")+geom_point(color="#E69F00")+ylab(expression(X[n]))+ ggtitle(expression(paste("Représentation du processus")))+
annotate("text", x=100, y =1000, label = expression(paste(alpha,"=5  ", beta,"=7  ",gamma,"=15    ")))





