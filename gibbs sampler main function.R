gibbs.LDA.cov=function(ncomm,ngibbs,nburn,y,phi.prior,a.gamma,b.gamma){
  #basic settings
  nloc=nrow(y)
  nspp=ncol(y)
  
  #initial values
  array.lsk=array(0,dim=c(nloc,nspp,ncomm))
  for (i in 1:nloc){
    for (j in 1:nspp){
      if (y[i,j]!=0){
        array.lsk[i,j,]=rmultinom(1,size=y[i,j],prob=rep(1/ncomm,ncomm))  
      }
    }
  }

  nlk=apply(array.lsk,c(1,3),sum)
  nks=t(apply(array.lsk,2:3,sum))
  nk=colSums(nlk)
  
  phi=matrix(1/nspp,ncomm,nspp)  
  lambda=matrix(1,nloc,ncomm)

  #to store outcomes from gibbs sampler
  lambda.out=matrix(NA,ngibbs,ncomm*nloc)
  phi.out=matrix(NA,ngibbs,nspp*ncomm)
  nlk.out=matrix(NA,ngibbs,nloc*ncomm)
  llk.out=rep(NA,ngibbs)

  #useful stuff for MH algorithm
  accept.output=50
  nadapt=ngibbs/2
  
  #run gibbs sampler
  options(warn=2)
  for (i in 1:ngibbs){
    print(i)

    #sample z
    tmp = SampleArray(Arraylsk=array.lsk, nloc=nloc,nspp=nspp,ncomm=ncomm,
                      y=y,runif1=runif(sum(y)),
                      LogPhi=log(phi), LogLambda=log(lambda))
    array.lsk=tmp$ArrayLSK
    # array.lsk=array.lsk.true
    nlk=apply(array.lsk,c(1,3),sum)
    nks=t(apply(array.lsk,2:3,sum))
    # nks=nks.true#rbind(nks.true,0,0)
    # nlk=nlk.true#cbind(nlk.true,0,0)

    #sample thetas
    lambda=sample.lambda(nlk=nlk,a.gamma=a.gamma,b.gamma=b.gamma,nloc=nloc,ncomm=ncomm)
    # lambda=media.true#c(lambda.true,0.01,0.01)
    
    #sample phi
    phi=rdirichlet1(alpha=nks+phi.prior,ncomm=ncomm,nspp=nspp)
    # phi=phi.true #rbind(phi.true,0,0)

    #calculate Poisson probabilities
    p1=dpois(nlk,lambda,log=T)
    # phi.tmp=phi; phi.tmp[phi.tmp<0.00001]=0.00001
    
    p2=LogLikMultin(nloc=nloc,ncomm=ncomm,nspp=nspp,phi=phi,Arraylsk=array.lsk)    
    
    #get phi prior
    p3=ldirichlet(x=phi,alpha=phi.prior)
    # log(ddirichlet(phi[2,],rep(phi.prior,nspp)))
    
    #get lambda prior
    p5=dgamma(lambda,a.gamma,b.gamma,log=T)
    
    #store results  
    llk.out[i]=sum(p1)+sum(p2)+sum(p3)+sum(p5)
    phi.out[i,]=phi
    lambda.out[i,]=lambda
    nlk.out[i,]=nlk
  }

  list(llk=llk.out,phi=phi.out,lambda=lambda.out,nlk=nlk.out)  
}


