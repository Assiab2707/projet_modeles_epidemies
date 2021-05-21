library(ggplot2)
simu_1_model<- function(H,alpha,beta,gamma){
  X<-NULL
  X[1]<-0
  proba<-c(alpha/(alpha+beta+gamma),beta/(alpha+beta+gamma),gamma/(alpha+beta+gamma))
  U<-sample(c(-1,0,1),H,replace=TRUE, prob=proba)
  for (i in 1:H){
    X[i+1]<-(X[i]+U[i])*(X[i]>0)+ (X[i]==0)*(U[i]==1)
  }
  simu<-data.frame(0:1000,X)
  colnames(simu)<-c("n","X_n")
  return(simu)
}
#Récurrente positive
simu_1<-simu_1_model(1000,60,30,40)
simu_1bis<-simu_1_model(1000,60,40,30)
ggplot(data=simu_1,aes(n,X_n)) + geom_line(color="#E69F00")+geom_point(color="#E69F00")+ylab(expression(X[n]))+ ggtitle(expression(paste("Représentation du processus dans le cas où ", (X[n])[n], " est récurrente positive")))+
annotate("text", x=100, y =17, label = expression(paste(alpha,"=60    ", beta,"=30    ",gamma,"=40    ")))
ggplot(data=simu_1bis,aes(n,X_n)) + geom_line(color="#E69F00")+geom_point(color="#E69F00")+ylab(expression(X[n]))+ ggtitle(expression(paste("Représentation du processus dans le cas où ", (X[n])[n], " est récurrente positive")))+
annotate("text", x=100, y =6, label = expression(paste(alpha,"=60    ", beta,"=40    ",gamma,"=30    ")))

#Transitoire
simu_2<-simu_1_model(1000,20,30,50)
simu_2bis<-simu_1_model(1000,30,20,50)
ggplot(data=simu_2,aes(n,X_n)) + geom_line(color="#33CC66")+geom_point(color="#33CC66")+ylab(expression(X[n]))+ ggtitle(expression(paste("Représentation du processus dans le cas où ", (X[n])[n], " est transitoire")))+
annotate("text", x=100, y =310, label = expression(paste(alpha,"=20    ", beta,"=30    ",gamma,"=50    ")))
ggplot(data=simu_2bis,aes(n,X_n)) + geom_line(color="#33CC66")+geom_point(color="#33CC66")+ylab(expression(X[n]))+ ggtitle(expression(paste("Représentation du processus dans le cas où ", (X[n])[n], " est transitoire")))+
annotate("text", x=100, y =230, label = expression(paste(alpha,"=30    ", beta,"=20    ",gamma,"=50    ")))

#Récurrente nulle
simu_3<-simu_1_model(1000,30,50,30)
simu_3bis<-simu_1_model(1000,40,10,40)
ggplot(data=simu_3,aes(n,X_n)) + geom_line(color="#6666FF")+geom_point(color="#6666FF")+ylab(expression(X[n]))+ ggtitle(expression(paste("Représentation du processus dans le cas où ", (X[n])[n], " est récurrente nulle")))+
annotate("text", x=100, y =28, label = expression(paste(alpha,"=30    ", beta,"=50    ",gamma,"=30    ")))
ggplot(data=simu_3bis,aes(n,X_n)) + geom_line(color="#6666FF")+geom_point(color="#6666FF")+ylab(expression(X[n]))+ ggtitle(expression(paste("Représentation du processus dans le cas où ", (X[n])[n], " est récurrente nulle")))+
annotate("text", x=100, y =33, label = expression(paste(alpha,"=40    ", beta,"=10    ",gamma,"=40    ")))

