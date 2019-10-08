plot(res$llk[1:ngibbs],type='l')

compare1=function(estim,true){
  rango=range(c(true,estim))
  plot(true,estim,ylim=rango,xlim=rango)
  lines(rango,rango)  
}

#look at nlk
tmp=matrix(res$nlk[ngibbs,],nloc,ncomm); 
tmp[1:10,]
ordem=c(7,4,1,6,3,5,2,8)
tmp[1:8,ordem]

boxplot(tmp)
compare1(estim=jitter(tmp[,ordem]),true=jitter(nlk.true))

#look at phi
tmp=matrix(res$phi[ngibbs,],ncomm,nspp)
tmp1=tmp[ordem,]
compare1(estim=tmp1,true=phi.true)
