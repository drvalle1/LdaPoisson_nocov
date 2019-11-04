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

#basic settings
ngibbs=1000
nburn=ngibbs/2
phi.prior=0.01
a.gamma=b.gamma=0.1

waic=rep(NA,15)
for (ncomm in 3:15){
  res=gibbs.LDA.nocov(ncomm=ncomm,ngibbs=ngibbs,nburn=nburn,y=y,
                      phi.prior=phi.prior,a.gamma=a.gamma,b.gamma=b.gamma)

  #calculate WAIC
  # plot(res$llk,type='l')
  seq1=nburn:ngibbs
  tmp=res$llk.ind.out[seq1,]
  p1=-2*sum(log(colMeans(exp(tmp))))
  p2=2*sum(apply(tmp,2,var))
  waic[ncomm]=p1+p2;
}

par(mfrow=c(1,1))
plot(waic,type='l')