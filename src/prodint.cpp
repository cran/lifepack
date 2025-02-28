#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
arma::mat prodint_cpp(Function A, double s, double t, int n) {
  double h = (t - s) / n;
  double x0 = s;

  // Initialiser identitetsmatrice med samme dimension som A(s)
  arma::mat y0 = arma::eye(Rcpp::as<arma::mat>(A(s)).n_rows,
                           Rcpp::as<arma::mat>(A(s)).n_cols);

  // Runge-Kutta 4. ordens metode
  for (int i = 0; i < n; i++) {
    arma::mat A0 = Rcpp::as<arma::mat>(A(x0));
    arma::mat s1 = h * y0 * A0;
    arma::mat s2 = h * (y0 + s1 / 2) * Rcpp::as<arma::mat>(A(x0 + h / 2));
    arma::mat s3 = h * (y0 + s2 / 2) * Rcpp::as<arma::mat>(A(x0 + h / 2));
    arma::mat s4 = h * (y0 + s3) * Rcpp::as<arma::mat>(A(x0 + h));

    y0 += (s1 / 6) + (s2 / 3) + (s3 / 3) + (s4 / 6);
    x0 += h;
  }

  return y0;
}
