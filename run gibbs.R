# rm(list=ls(all=TRUE))
library(MCMCpack)
library('Rcpp')
library('RcppArmadillo')
set.seed(10)

#get functions
setwd('U:\\GIT_models\\LdaPoisson_nocov')
source('gibbs sampler main function.R')
source('gibbs functions.R')
sourceCpp('aux1.cpp')

#get data
dat=read.csv('fake data8.csv',as.is=T)
y=data.matrix(dat)

#basic settings
ncomm=5
ngibbs=1000
nburn=ngibbs/2
phi.prior=0.01
a.gamma=b.gamma=0.1

res=gibbs.LDA.cov(ncomm=ncomm,ngibbs=ngibbs,nburn=nburn,y=y,
                  phi.prior=phi.prior,a.gamma=a.gamma,b.gamma=b.gamma)

plot(res$lambda[ngibbs,],type='h')
plot(res$llk,type='l')