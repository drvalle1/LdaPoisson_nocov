// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <iostream>
#include <ctime>
#include <fstream>
using namespace Rcpp;

/***************************************************************************************************************************/
/*********************************                      UTILS          *****************************************************/
/***************************************************************************************************************************/

// This function helps with multinomial draws
// [[Rcpp::export]]
int whichLessDVPresence(double value, NumericVector prob) {
  int res=prob.length()-1;
  double probcum = 0;
  
  for (int i = 0; i < prob.length(); i++) {
    probcum = probcum + prob(i);
    if (value < probcum) {
      res = i;
      break;
    }
  }
  return res;
}

// Get lambda
// [[Rcpp::export]]
NumericVector GetLambda(NumericVector LogTheta, int ncomm) {
  NumericVector res(ncomm);
  res[0]=LogTheta[0];
  for (int i = 1; i < ncomm; i++) {
    res[i]=res[i-1]+LogTheta[i];
  }
  res=exp(res);
  return res;
}

// This function calculates the multinomial distribution
// [[Rcpp::export]]
double ldmultinom(IntegerVector x, int size, NumericVector prob) {
  NumericVector x1(x.length());
  
  //convert from integer to numeric
  for (int i = 0; i < x.length(); i++){
    x1[i]=(double) x[i];
  } 
  
  NumericVector res1=x1*log(prob);
  double res=lgamma(size+1)+sum(res1 - lgamma(x+1));
  return(res);
}


// this function calculates the loglikelih based on multinomial
// [[Rcpp::export]]
NumericVector LogLikMultin(int nloc, NumericMatrix prob, IntegerVector tot, IntegerMatrix y){
  NumericVector p2(nloc);
  for (int l = 0; l < nloc; l++) {
    p2[l]=ldmultinom(y(l,_),tot[l],prob(l,_));
  }
  return(p2);
}

// This function samples Array.lsk
// [[Rcpp::export]]

List SampleArray(NumericVector Arraylsk, int nloc, int nspp, int ncomm,
                 IntegerMatrix y, NumericVector runif1, 
                 NumericMatrix LogPhi, NumericMatrix LogLambda){
  
  //convert array into arma::cube
  NumericVector vecArray=clone(Arraylsk);
  arma::cube ArrayLSK1(vecArray.begin(), nloc, nspp, ncomm, false);
  arma::cube ArrayLSK1Orig(Arraylsk.begin(), nloc, nspp, ncomm, false);

  //initialize stuff
  NumericVector prob(ncomm);
  int ind;
  int oo=0;
  
  for (int l = 0; l < nloc; l++) {
    //go over each individual (i.e., each element of ArrayLSK1Orig)
    for (int s = 0; s < nspp; s++){
      if (y(l,s)>0){
        for (int k = 0; k < ncomm; k++){
          if (ArrayLSK1Orig(l,s,k)>0){
            for (int i = 0; i < ArrayLSK1Orig(l,s,k); i++){
              //remove i-th individual
              ArrayLSK1(l,s,k)=ArrayLSK1(l,s,k)-1;

              //calculate assignment probabilities
              for (int k1 = 0; k1 < ncomm; k1++){
                prob[k1]=LogPhi(k1,s)+LogLambda(l,k1)-log(ArrayLSK1(l,s,k1)+1);  
              }
              prob=prob-max(prob);
              prob=exp(prob);
              prob=prob/sum(prob);
              
              //sample cluster membership of i-th individual
              ind=whichLessDVPresence(runif1[oo], prob);
              oo=oo+1;
              
              //update counts
              ArrayLSK1(l,s,ind)=ArrayLSK1(l,s,ind)+1;
            }
          }
        }
      }
    }
  }

  List L = List::create(Named("ArrayLSK") =ArrayLSK1);
  
  return(L);
}
