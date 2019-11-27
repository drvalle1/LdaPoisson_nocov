plot(res$llk[1:ngibbs],type='l')

compare1=function(estim,true){
  rango=range(c(true,estim))
  plot(true,estim,ylim=rango,xlim=rango)
  lines(rango,rango)  
}

#look at nlk
nlk.estim=apply(res$array.lsk,c(1,3),sum)
round(nlk.estim[1:20,])
ordem=c(5,3,2,1,4)
round(tmp1[1:10,ordem])

boxplot(tmp1)
nlk.estim=tmp1[,ordem]
compare1(estim=jitter(nlk.estim),true=jitter(nlk.true))

#look at phi
tmp=matrix(res$phi[ngibbs/2,],ncomm,nspp)
tmp1=tmp[ordem,]
compare1(estim=tmp1,true=phi.true)

#look at lambda
tmp=matrix(res$lambda[ngibbs,],nloc,ncomm)
tmp1=tmp[,ordem]
compare1(estim=tmp1,true=media.true)

#look at betas
tmp=read.csv('fake data xmat.csv',as.is=T)
xmat=data.matrix(tmp)
betas=matrix(NA,ncol(xmat)+1,ncomm)
for (i in 1:ncomm){
  dat.tmp=cbind(nlk.estim[,i],xmat)
  colnames(dat.tmp)=c('y',paste0('cov',1:ncol(xmat)))
  dat.tmp1=as.data.frame(dat.tmp)
  res=glm(y~.,data=dat.tmp1,family='poisson')
  betas[,i]=res$coef
}
round(betas[-1,],3)

