#include <Rcpp.h>
using namespace Rcpp;
//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param N the number of samples
//' @param thin the number of between-sample random numbers
//' @return a random sample of size \code{n}
//' @importFrom stats rbinom
//' @importFrom stats rbeta
//' @examples
//' \dontrun{
//' rnC <- gibbsC(100,10)
//' par(mfrow=c(2,1));
//' plot(rnC[,1],type='l')
//' plot(rnC[,2],type='l')
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix gibbsC(int N, int thin) {
  NumericMatrix X(N,2);
  NumericMatrix Y(N-thin,2);
  int a = 2;
  int b = 3;
  int n = 10;
  double mu1 = 2;
  double mu2 = 0.5;
  X(0,0) = mu1;
  X(0,1) = mu2;
  for (int i = 1; i < N; i++) {
    X(i,0) = R::rbinom(n,X(i-1,1));
    X(i,1) = R::rbeta(X(i-1,0)+a, n-X(i-1,0)+b);
    if(i >= thin){
      Y(i-thin, 0) = X(i, 0);
      Y(i-thin, 1) = X(i, 1);
      }
  }
  return(Y);
}

