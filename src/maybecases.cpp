#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
IntegerMatrix equalpairs(NumericVector u, NumericVector v, IntegerVector ou, IntegerVector ov, int max_number_of_pairs){
  int nu = u.size();
  int nv =v.size();

  IntegerMatrix pairs(2, max_number_of_pairs);
  int n = 0;

  // Sorting the v vector
  NumericVector sorted_v=clone(v);
  std::sort(sorted_v.begin(), sorted_v.end());

  // Set precision epsilon
  double eps = 0.0000001;

  for (int i=0; i<nu; ++i) {
    //  lower bound using binary search
    int lower_bound = std::lower_bound(sorted_v.begin(), sorted_v.end(), u[i] - eps) - sorted_v.begin();

    // Checking if the element at the lower bound is close to u[i]
    if (std::abs(sorted_v[lower_bound] - u[i]) < eps) {
      while (lower_bound < nv && std::abs(sorted_v[lower_bound] - u[i]) < eps && n < max_number_of_pairs) {
        // Adding pairs that are close
        pairs(0, n) = ou[i];

        //troubling/confusing bit->to be worked on
        pairs(1, n) = ov[std::find(v.begin(), v.end(), sorted_v[lower_bound])-v.begin()];
        n++;
        lower_bound++;
      }
    }
  }

  if (n > 0) {
    IntegerMatrix result(2, n);
    for (int i = 0; i < n; ++i) {
      result(0, i) = pairs(0, i);
      result(1, i) = pairs(1, i);
    }
    return result;
  }

  IntegerMatrix empty_result(2, 1);
  return empty_result;
}
