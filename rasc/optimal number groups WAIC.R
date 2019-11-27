rm(list=ls(all=TRUE))
library(MCMCpack)
library('Rcpp')
library('RcppArmadillo')
set.seed(11)

#get functions
setwd('U:\\GIT_models\\LdaPoisson_nocov')
source('LdaPoisson_nocov main function.R')
source('LdaPoisson_nocov aux functions.R')
sourceCpp('LdaPoisson_nocov_aux_cpp.cpp')

#get data
dat=read.csv('fake data.csv',as.is=T)
y=data.matrix(dat)
nloc=nrow(y)
nspp=ncol(y)

#basic settings
ngibbs=1000
nburn=ngibbs/2
phi.prior=0.01
a.gamma=b.gamma=0.1

waic=llk=rep(NA,15)
aic=matrix(NA,3,15)
for (ncomm in 3:15){
  res=gibbs.LDA.nocov(ncomm=ncomm,ngibbs=ngibbs,nburn=nburn,y=y,
                      phi.prior=phi.prior,a.gamma=a.gamma,b.gamma=b.gamma)

  #look at convergence
  plot(res$llk,type='l',main=ncomm)
  
  #get WAIC
  seq1=nburn:ngibbs
  nsim=length(seq1)
  tmp=res$llk.ind.out[seq1,]
  max1=apply(tmp,2,max)
  tmp1=tmp-matrix(max1,nsim,nloc,byrow=T)
  tmp2=log(colSums(exp(tmp1)))
  tmp3=max1+tmp2-log(nsim)                  
  p1=-2*sum(tmp3)
  p2=2*sum(apply(tmp,2,var))
  waic[ncomm]=p1+p2;
  
  #get llk
  llk[ncomm]=mean(res$llk[seq1])
  
  #get AIC
  nparam=ncomm*nloc+(nspp*(ncomm-1))
  aic[,ncomm]=2*nparam-2*quantile(res$llk[seq1],c(0.025,0.5,0.975))
}

par(mfrow=c(3,1))
plot(waic,type='l',main='waic')
plot(x=1:15,aic,type='l',main='aic')
plot(llk,type='l',main='llk')

par(mfrow=c(1,1))
plot(NA,NA,xlim=c(1,15),ylim=range(aic),main='aic')
points(x=1:15,y=aic[2,])
for (i in 3:15){
  lines(x=rep(i,2),y=aic[c(1,3),i])
}