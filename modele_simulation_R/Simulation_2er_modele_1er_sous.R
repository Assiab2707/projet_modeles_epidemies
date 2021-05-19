N=1000
simu_2_model<- function(H,alpha,beta,gamma){
  X<-NULL
  X[1]<-1
  for (i in 1:H){
    proba<-c(X[i]*alpha/(X[i]*alpha+beta+gamma),beta/(X[i]*alpha+beta+gamma),gamma/(X[i]*alpha+beta+gamma))
    U<-sample(c(-1,0,1),H,replace=TRUE, prob=proba)
    X[i+1]<-(X[i]+U[i])*(X[i]>0)+ (X[i]==0)*(U[i]==1)
  }
  simu<-data.frame(0:1000,X)
  colnames(simu)<-c("n","X_n")
  return(simu)
}

#Récurrente positive
simu_4<-simu_2_model(N,10,30,50)
simu_4bis<-simu_2_model(N,15,10,10)
ggplot(data=simu_4,aes(n,X_n)) + geom_line(color="#E69F00")+geom_point(color="#E69F00")+ylab(expression(X[n]))+ ggtitle(expression(paste("Représentation du processus ", (X[n])[n])))+
  annotate("text", x=100, y =12, label = expression(paste(alpha,"=10    ", beta,"=30    ",gamma,"=50    ")))
ggplot(data=simu_4bis,aes(n,X_n)) + geom_line(color="#E69F00")+geom_point(color="#E69F00")+ylab(expression(X[n]))+ ggtitle(expression(paste("Représentation du processus ", (X[n])[n])))+
  annotate("text", x=100, y =5.3, label = expression(paste(alpha,"=15    ", beta,"=10    ",gamma,"=10    ")))




