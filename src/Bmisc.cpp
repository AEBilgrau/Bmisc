// We only include RcppArmadillo.h which pulls Rcpp.h in for us
#include <RcppArmadillo.h>

//using namespace Rcpp;
//using namespace RcppArmadillo;

//// [[Rcpp::depends(RcppArmadillo)]]

//' @rdname MultivariateSpecial
//' @return \code{lgammap} is the log multivariate gamma function. 
//' @note For \code{lgammp}, warnings of the type \code{"value out of range in 
//'   'lgamma'"} is due to evaluation in the half integers below \eqn{(p-1)/2}.
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector lgammap(const Rcpp::NumericVector & x, const int p = 1) {
  const double c0 = log(M_PI)*p*(p - 1)/4;
  Rcpp::NumericVector ans(x.size(), c0);
  for (int j = 0; j < p; ++j) {
    ans += Rcpp::lgamma(x - j/2.0f);
  }
  return ans;
}



/*** R



*/

