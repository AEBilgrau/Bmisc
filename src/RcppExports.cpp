// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// lgammap
Rcpp::NumericVector lgammap(Rcpp::NumericVector& x, const int p = 1);
RcppExport SEXP Bmisc_lgammap(SEXP xSEXP, SEXP pSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type x(xSEXP );
        Rcpp::traits::input_parameter< const int >::type p(pSEXP );
        Rcpp::NumericVector __result = lgammap(x, p);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
