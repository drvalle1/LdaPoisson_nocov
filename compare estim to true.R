plot(res$llk[1:ngibbs],type='l')

compare1=function(estim,true){
  rango=range(c(true,estim))
  plot(true,estim,ylim=rango,xlim=rango)
  lines(rango,rango)  
}

#look at nlk
seq1=(ngibbs/2):ngibbs
tmp=colMeans(res$nlk[seq1,])
tmp1=matrix(tmp,nloc,ncomm); 
round(tmp1[1:10,])
ordem=c(1,5,4,2,3)
round(tmp1[1:10,ordem])

boxplot(tmp1)
compare1(estim=jitter(tmp1[,ordem]),true=jitter(nlk.true))

#look at phi
tmp=matrix(res$phi[ngibbs,],ncomm,nspp)
tmp1=tmp[ordem,]
compare1(estim=tmp1,true=phi.true)

#look at lambda
tmp=matrix(res$lambda[ngibbs,],nloc,ncomm)
tmp1=tmp[,ordem]
compare1(estim=tmp1,true=media.true)
