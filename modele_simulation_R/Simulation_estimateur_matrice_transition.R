N=100000
alpha = 5
beta = 10
gamma = 10
  
simu_2_model<- function(H,alpha,beta,gamma){
  X<-NULL
  X[1]<-0
  for (i in 1:H){
    proba<-c(X[i]*alpha/(X[i]*alpha+beta+gamma),beta/(X[i]*alpha+beta+gamma),gamma/(X[i]*alpha+beta+gamma))
    U<-sample(c(-1,0,1),H,replace=TRUE, prob=proba)
    X[i+1]<-(X[i]+U[i])*(X[i]>0)+ (X[i]==0)*(U[i]==1)
  }
  return(X)
}

#Récurrente positive
mod1<-simu_2_model(N,alpha,beta,gamma)
plot(mod1,type="o",col="blue",xlab='n',ylab=expression(X[n]), pch=16)

pij_hat<- function(simu,n,i,j){
  p_ij<-NULL
  num<-0
  denum<-0
  for (k in 1:n){
    num<- num+as.numeric(simu[k]==i && simu[k+1]==j)
    denum<- denum + (as.numeric(simu[k]==i))
    p_ij[k]<- num/denum
  }
  return(p_ij)
}

p_00<- pij_hat(mod1,N,0,0)
plot(p_00,type="o",col="blue",xlab='n',ylab=expression(p["00"]), pch=16)
abline(h=beta/(beta+gamma))

p_01<- pij_hat(mod1,N,0,1)
plot(p_01,type="o",col="blue",xlab='n',ylab=expression(p["01"]), pch=16)
abline(h=gamma/(beta+gamma))


p_10<- pij_hat(mod1,N,1,0)
plot(p_10,type="o",col="blue",xlab='n',ylab=expression(p["10"]), pch=16)
abline(h=alpha/(alpha+beta+gamma))

