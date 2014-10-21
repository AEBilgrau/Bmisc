// We only include RcppArmadillo.h which pulls Rcpp.h in for us
#include <RcppArmadillo.h>
//#include <math.h>
//#include <vector>

//using namespace Rcpp;
//using namespace RcppArmadillo;

//// [[Rcpp::depends(RcppArmadillo)]]

////' Compute log determinant
////' 
////' Compute the sign and log of the determinant. Faster than \code{determinant}.
////' 
////' @param x A numeric matrix.
////' @return Returns a 1 by 2 matrix where the first element is the log of the 
////'  determinant and the second element is the sign of the determinant.
////' @seealso \code{\link{determinant}}
////' @author Anders Ellern Bilgrau
////' @examples
////' y <- matrix(rnorm(100), 10, 10)
////' determinant(y)
////' Bmisc:::logdetArma(y)
//// [[Rcpp::export]]
//arma::vec logdetArma(const arma::mat & x) {
//  arma::vec val_sign(2); // First element value and second element is sign
//  log_det(val_sign(0), val_sign(1), x);
//  return val_sign;
//}

//' @rdname MultivariateSpecial
//' @return \code{lgammap} is the log multivariate gamma function. 
//' @note For \code{lgammp}, warnings of the type \code{"value out of range in 
//'   'lgamma'"} is due to evaluation in the half integers below \eqn{(p-1)/2}.
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector lgammap(Rcpp::NumericVector & x, const int p = 1) {
  double c0 = log(M_PI)*p*(p - 1)/4;
  Rcpp::NumericVector ans(x.size(), c0);
  for (int j = 0; j < p; ++j) {
    ans += Rcpp::lgamma(x - j/2.0f);
  }
  return ans;
}


//// [[Rcpp::export]]
//double loglikArma(const arma::mat & Psi, const double nu, 
//                  const Rcpp::List & S_list,  const arma::vec & ns) { 
//  const int k = S_list.size();
////  const arma::mat S0 = S_list[0];
//  const int p = Psi.n_rows;
//  const arma::vec cs = (nu + ns)/2;
//  const double logdetPsi = logdetArma(Psi)(0);
//  
//  arma::vec logdetPsiPlusS(k);
//  for (int i = 0; i < k; ++i) {
//    arma::mat Si = S_list[i];
//    logdetPsiPlusS(i) = logdetArma( Psi + Si )(0);
//  }
//
////  const1 <- sum(((ns * p)/2) * log(2))
////  t1 <- k*nu/2 * logdetPsi
////  t2 <- sum(lgammap(cs, p = p))
////  t3 <- -sum(cs * logdetPsiPlusS)
////  t4 <- -k*lgammap(nu/2, p = p)
////  const1 + t1 + t2 + t3 + t4
//  return 1;
//}
//
//// [[Rcpp::export]]
//arma::mat lgamma_arma(const arma::mat & x) {
//  const int n = x.n_rows, m = x.n_cols;
//  arma::mat ans(n, m, arma::fill::zeros);
//  for (int i = 0; i < n; ++i) {
//      for (int j = 0; j < m; ++j) {
//        ans(i,j) = lgamma(x(i,j));
//      }
//  }
//  return ans;
//}
//
//// [[Rcpp::export]]
//Rcpp::NumericMatrix lgamma_rcpp_mat(Rcpp::NumericMatrix x) {
//  for (int i = 0; i < x.nrow(); ++i) {
//    x(Rcpp::_, i) = lgamma(x(Rcpp::_, i));
//  }
//  return x;
//}
//
//// [[Rcpp::export]]
//Rcpp::NumericVector lgamma_rcpp_vec(Rcpp::NumericVector & x) {
//  return Rcpp::lgamma(x);
//}



/*** R

library(Bmisc)
library(microbenchmark)
x <- matrix(rnorm(1100), 110, 10)

microbenchmark(determinant(crossprod(x)),
               Bmisc:::logdetArma(crossprod(x)))
y <- matrix(rnorm(100), 10, 10)
determinant(y)
Bmisc:::logdetArma(y)

yy <- seq(-10, 10, by = 0.01)

all.equal(Bmisc:::lgammap_old(yy, p = 20), Bmisc:::lgammap(yy, p = 20))
all.equal(Bmisc:::lgammap_old(yy, p = 20), lgammap(yy, p = 20))
microbenchmark(lgammap(yy), lgammap_old(yy))

*/

