#include <Rcpp.h>
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
  // Sampling with/o replacement
  indices1 = Rcpp::sample(v, M, with_replacement);

  for (int i = 0; i < M; ++i) {
    indices1[i] = indices1[i] - 1;
  }

  for (int i = 0; i < n; ++i) {
    IntegerVector ind = findIndex(indices1, i);
    float sum1 = 0.0; // Declare and initialize sum1 here
    if(ind.size()==1){
      R[i]=D[ind[0]];
    }
    else{
      for(int j = 0; j < ind.size(); ++j) {
      sum1 += D[ind[j]];
    }
      R[i] = sum1;}

  }

  return R;
}


//[[Rcpp::export]]
IntegerMatrix equalpairs(NumericVector u, NumericVector v, IntegerVector ou, IntegerVector ov, int max_number_of_pairs) {
  //set sizes of array
  int nu = u.size();
  int nv = v.size();

  //init two lists to store pairs
  std::list<int> pairs_u;
  std::list<int> pairs_v;

  //set pointers
  int start = 0;
  int j = 0;

  //set counter
  int count = 0;

  //set precision epsilon
  double eps = 0.0000001;

  //start looping through u vector
  for(int i = 0; i < nu; ++i) {

    //increase if too small
    while(v[start]<u[i]-eps && start < nv-1) {
      ++start;
    }

    //if close consider the pairs that might be close
    if(std::abs(v[start]-u[i]) < eps) {
      j = start;
      while(std::abs(v[j]-u[i]) < eps) {
        //add pairs that are close
        pairs_u.push_front(ou[i]);
        pairs_v.push_front(ov[j]);

        ++j;
        ++count;
        if(j >= nv) {
          break;
        }
      }
    }
    //if there are too many pairs kill the search
    if(count > max_number_of_pairs) {
      break;
    }
  }
  int n = 0;
  //fill pairs in a 2x|pairs| matrix
  if(pairs_u.size() > 0) {
    IntegerMatrix pairs(2,pairs_u.size());
    while(!pairs_u.empty()) {
      pairs(0,n)=pairs_u.back();
      pairs_u.pop_back();
      pairs(1,n)=pairs_v.back();
      pairs_v.pop_back();
      ++n;
    }
    return pairs;
  }
  IntegerMatrix pairs(2,1);
  return pairs;
}


// function to calculate the interaction strength γjk for pair (j, k)
// [[Rcpp::export]]
double interaction_strength(NumericMatrix X, NumericVector Y, int j, int k) {
  int n = X.nrow();
  int p = X.ncol();
  int count = 0;
  double sum = 0.0;

  // calculate Z(n x p) matrix
  NumericMatrix Z(n, p);
  for (int i=0; i<n; ++i) {
    for (int l=0; l < p;++l) {
      Z(i,l) = Y[i]*X(i, l);
    }
  }

  // Calculate the interaction strength gamma_jk
  for (int i = 0; i < n; ++i) {
    // Check if Yi = Xij * Xik
    if (Y[i] == X(i, j) * X(i, k)) {
      sum += 1.0;
    }
  }

  // retrn it
  return sum / n;
}


