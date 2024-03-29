rm(list=ls(all=TRUE))
library(MCMCpack)
library('Rcpp')
library('RcppArmadillo')
set.seed(12)

#get data
setwd('U:\\GIT_models\\LdaPoisson_nocov')
dat=read.csv('fake data.csv',as.is=T)
xmat=data.matrix(read.csv('fake data xmat.csv',as.is=T))
y=data.matrix(dat)

#basic settings
ncomm=10
ngibbs=1000
nburn=ngibbs/2

#priors
psi=phi.prior=0.01
a.gamma=b.gamma=0.1
var.betas=10
gamma=0.1
#----------------------------------------------------------
#run LDA no covariates to get initial values

#get functions
setwd('U:\\GIT_models\\git_LDA_abundance')
source('gibbs functions.R')
source('LDA.abundance main function.R')
sourceCpp('aux1.cpp')

res=LDA.abundance(y=y,ncomm=ncomm,ngibbs=ngibbs,nburn=nburn,psi=psi,gamma=gamma)

nloc=nrow(y)
nspp=ncol(y)
array.lsk.init=res$array.lsk

#determine optimal number of groups
nlk=apply(array.lsk.init,c(1,3),sum)
theta=nlk/apply(nlk,1,sum)
par(mfrow=c(1,1),mar=c(3,3,1,1))
boxplot(theta)