#this function generates dirichlet random variables (1 one for each row of alpha)
rdirichlet1=function(alpha,ncomm,nspp){
  tmp=matrix(rgamma(n=ncomm*nspp,alpha,1),ncomm,nspp)
  soma=matrix(rowSums(tmp),ncomm,nspp)
  tmp/soma
}
#---------------------------------------------
ldirichlet=function(x,alpha){
  n=ncol(alpha)
  tmp=rowSums((alpha-1)*log(x))
  invBeta1=lgamma(alpha*n)-n*lgamma(alpha)
  invBeta1+tmp
}
#---------------------------------------------
ldmultinom1=function(size,x,prob){
  lgamma(size + 1) + sum(x * log(prob) - lgamma(x + 1))
}
#---------------------------------------------
sample.lambda=function(nlk,a.gamma,b.gamma,nloc,ncomm){
  tmp=rgamma(nloc*ncomm,nlk+a.gamma,1+b.gamma)
  matrix(tmp,nloc,ncomm)
}