#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector findIndex(NumericVector array, double value) {
  IntegerVector indices;

  for (int i = 0; i < array.size(); ++i) {
    if (array[i] == value) {
      indices.push_back(i); // Convert 0-based to 1-based index
    }
  }

  return indices;
}

// [[Rcpp::export]]
NumericVector generate_uniform_values(int n, double a = 0.0, double b = 1.0) {
  NumericVector result(n);

  // Generate random values from U[a, b]
  for (int i = 0; i < n; ++i) {
    result[i] = R::runif(a, b);
  }

  return result;
}



// [[Rcpp::export]]
NumericVector generate_random_projection(int n, int M, bool with_replacement) {
  NumericVector D = Rcpp::runif(M); // Generate vector D with independent components U[0, 1]

  NumericVector R(n); // Initialize vector R with zeros
  NumericVector indices1(M);
  IntegerVector v = seq(1,n);

  if (with_replacement) {
    // Sampling with replacement
    indices1 = Rcpp::sample(v, M, true); // Sample M indices with replacement
  } else {
    indices1 = Rcpp::sample(v, M, false); // Sample M indices without replacement
  }

  for (int i = 0; i < M; ++i) {
    indices1[i] = indices1[i] - 1;
  }

  for (int i = 0; i < n; ++i) {
    IntegerVector ind = findIndex(indices1, i);
    double sum1 = 0; // Declare and initialize sum1 here
    for (int j = 0; j <= ind.size(); ++j) {
      Rcout<<"l"<<ind.size();
      Rcout<<ind;
      sum1 += D[ind[j]];
      Rcout<<sum1;
    }
    R[i] = sum1;
  }

  return R;
}
