alpha_hat<- function(simu, H){
  alpha_n<-NULL
  num10<-0
  num00_01<-0
  denum10<-0
  denum00<-0
  for (i in 1:H){
    num10<- num10+as.numeric(simu[i]==1 && simu[i+1]==0)
    num00_01<-num00_01 + as.numeric(simu[i]==0 && simu[i+1]==0)+as.numeric(simu[i]==0 && simu[i+1]==1)
    denum10<- denum10 + (as.numeric(simu[i]==1)-as.numeric(simu[i]==1 && simu[i+1]==0))
    denum00<- denum00 + (as.numeric(simu[i]==0)-as.numeric(simu[i]==0 && simu[i+1]==0))
    alpha_n[i]<- (num10*num00_01)/(denum10*denum00)
  }
  return(alpha_n)
}
alpha_n<- alpha_hat(mod1,N)
plot(alpha_n,type="o",col="green",xlab='n',ylab=expression(alpha[n]), pch=16)
abline(h=alpha)


gamma_hat<- function(simu, n){
  gamma_n<-NULL
  num01<-0
  denum00<-0
  for (i in 1:n){
    num01<- num01+as.numeric(simu[i]==0 && simu[i+1]==1)
    denum00<- denum00 + (as.numeric(simu[i]==0)-as.numeric(simu[i]==0 && simu[i+1]==0))
    gamma_n[i]<- num01/denum00
  }
  return(gamma_n)
}


gamma_n<- gamma_hat(mod1,N)
plot(gamma_n, type="o",col="blue",xlab='n',ylab=expression(gamma[n]), pch=16)


