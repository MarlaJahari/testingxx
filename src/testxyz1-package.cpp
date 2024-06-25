#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector findIndex(NumericVector array, double value) {
  IntegerVector indices;

  for (int i = 0; i < array.size(); ++i) {
    if (array[i] == value) {
      indices.push_back(i);
    }
  }

  return indices;
}

// function to convert -1 to 0 and convert binary columns to integer values
// [[Rcpp::export]]
IntegerVector binaryToInt(NumericMatrix matrix) {
  int n = matrix.nrow();
  int p = matrix.ncol();
  IntegerVector result(p);

  for (int col = 0; col < p; col++) {
    int decimalValue = 0;
    for (int row = 0; row < n; row++) {
      if (matrix(row, col) == -1) {
        matrix(row, col) = 0; // Convert -1 to 0
      }
      decimalValue += matrix(row, col) * pow(2, n - row - 1);
    }
    result[col] = decimalValue;
  }

  return result;
}

// function to return the sorted index vector for an unsorted NumericVector
// [[Rcpp::export]]
IntegerVector sorted_index_vector(NumericVector v) {
  int N = v.size();
  std::vector<int> index_vec(N);
  std::iota(index_vec.begin(), index_vec.end(), 0);  // initialize with [0, 1, 2, ...]

  std::sort(index_vec.begin(), index_vec.end(),
            [&](int i, int j) { return v[i] < v[j] || (v[i] == v[j] && i < j); });

  // converting the std::vector<int> to IntegerVector before returning
  IntegerVector sorted_indexes(index_vec.begin(), index_vec.end());
  return sorted_indexes;
}


// [[Rcpp::export]]
NumericVector apply_permutation(NumericVector vec, IntegerVector p) {
  int n = vec.size();
  std::vector<bool> done(n);

  NumericVector result(n);

  for (int i = 0; i < n; ++i) {
    if (done[i]) {
      continue;
    }
    done[i] = true;
    int prev_j = i;
    int j = p[i];
    while (i != j) {
      result[j] = vec[prev_j];
      done[j] = true;
      prev_j = j;
      j = p[j];
    }
    result[j] = vec[prev_j];
  }

  return result;
}


// [[Rcpp::export]]
NumericVector combine(NumericVector x, NumericVector y) {
  int n = x.size();
  // append elements from y to the end of x
  for (int i=0; i <n; ++i) {
    x.push_back(y[i]);

  }
  return x;
}

// [[Rcpp::export]]
void push(IntegerMatrix M, IntegerVector new_vector) {
  int nrow = M.nrow();
  int ncol = M.ncol();
  int s = new_vector.size();

  if (s == ncol) { // push new_vector as an (n+1)th row
    IntegerMatrix new_matrix(nrow + 1, ncol);

    // cpy elements from the old matrix to the new matrix
    for (int i=0; i <nrow; ++i) {
      for (int j=0; j <ncol; ++j) {
        new_matrix(i,j) = M(i,j);
      }
    }

    // append the new vector as the last row of the new matrix
    for (int j=0; j<ncol; ++j) {
      new_matrix(nrow,j) = new_vector[j];
    }

    M= new_matrix;


  } else if (s == nrow) { // Push new_vector as a (p+1)th column
    IntegerMatrix new_matrix(nrow, ncol + 1);

    // copy elements from the old matrix to the new matrix
    for (int i= 0; i <nrow; ++i) {
      for (int j=0; j <ncol; ++j) {
        new_matrix(i,j) = M(i, j);
      }
    }

    // append the new vector as last column of the new matrix
    for (int i=0; i < nrow; ++i) {
      new_matrix(i, ncol) = new_vector[i];
    }

    M= new_matrix;


  } else { // an error for incompatible dimensions
    Rcpp::stop("Dimensions of the matrix and vector are not suitable for pushing.");
  }
}

//dont consider, should be optimised
// [[Rcpp::export]]
List find(IntegerVector v, int k) {
  std::sort(v.begin(), v.end());

  int n = v.size();
  List result;

  for (int i = 0; i < n; ++i) {
    int a = v[i];
    IntegerVector::iterator it = std::lower_bound(v.begin(), v.end(), a + k);

    if (it != v.end()) {
      int count = n - (it - v.begin());
      IntegerVector pairs(count);

      for (int j = it - v.begin(); j < n; ++j) {
        pairs[j - (it - v.begin())] = a;
        pairs[j - (it - v.begin()) + 1] = v[j];
      }

      result.push_back(pairs);
    }
  }

  return result;
}





// [[Rcpp::export]]
int findindexit(IntegerVector vec, double n) {
  IntegerVector::iterator it = std::lower_bound(vec.begin(), vec.end(), n);

  if (it != vec.end()) {
    return *it; // Return the found value
  } else {
    return -1; // No value found
  }
}

// [[Rcpp::export]]
int binarySearchIndex(IntegerVector v, int n) {
  int left=0;
  int right=v.size() - 1;

  while (left<=right) {
    int mid=left + (right - left) / 2;

    if (v[mid]==n) {
      return mid; // found, return its index
    } else if (v[mid]< n) {
      left=mid + 1;
    } else{
      right=mid - 1;
    }
  }

  return -1; // not found
}




// [[Rcpp::export]]
List splitVectors(ListOf<IntegerVector> inputList,int n) {
  int numVectors = inputList.size();
  List result;

  for (int i = 0; i < numVectors; i++) {

    IntegerVector vec = inputList[i];
    int s=vec.size();
    if (vec[0]<int(n/2) && vec[s-1]>=int(n/2)) {
     int m=std::upper_bound(vec.begin(), vec.end(), int(n/2)-1)- vec.begin();
     result.push_back(vec[Range(0, m-1)]);
     result.push_back(vec[Range(m, vec.size()-1)]);}
     // }
    }


  return result;
}




// function to create a matrix from two vectors similar to expand.grid in R
// [[Rcpp::export]]
IntegerMatrix expandGrid(IntegerVector vec1, IntegerVector vec2) {
  int len1 = vec1.size();
  int len2 = vec2.size();


  IntegerMatrix result(2,len1 * len2);
  result(0,_)= rep(vec1, len2);
  result(1,_)= rep(vec2, len1);

  return result;
}




// function to apply the cantor map to a 2*n matrix of pairs
// [[Rcpp::export]]
NumericVector cantor_map(NumericMatrix pairs) {
  int n = pairs.ncol();
  NumericVector result(n);

  for (int i=0; i<n; i++) {
    double x=pairs(0,i);
    double y=pairs(1,i);
    result[i]= 0.5*(x+y)*(x+y+1) +y;
  }

  return result;
}

// inverse Cantor map from a vector of integers
// [[Rcpp::export]]
NumericMatrix inverse_cantor_map(NumericVector values) {
  int n = values.size();
  NumericMatrix result(2, n);
  for (int i=0; i<n; i++) {
    double t= floor((-1 + sqrt(1+ 8*values[i]))/ 2);
    double w= values[i] - 0.5 *t*(t + 1);


    double x=floor(t-w);


    //double x= t-w;
    double y= floor(w);

    result(0,i)=x;
    result(1,i)=y;

  }

  return result;
}


// normalize a vector to have an L1 norm of 1
// [[Rcpp::export]]
NumericVector normalizeL1(NumericVector vec) {
  double sum=0.0;
  for (int i=0; i<vec.size(); ++i) {
    sum += std::abs(vec[i]);
    vec[i]=std::abs(vec[i]);
  }
  if (sum==0.0) {
    return vec; //no division by zero
  }
  return vec/sum;
}

// [[Rcpp::export]]
NumericVector transformY(NumericVector vec) {
  for (int i=0; i<vec.size(); ++i) {
    if (vec[i]<= 0) {vec[i] = 0;}
    else if(vec[i]==0){double ran=R::runif(0,1);vec[i] = (ran<0.5) ? 0 : 1;}
    else {  vec[i] = 1;}
  }
  return vec;
}

// [[Rcpp::export]]
IntegerVector getInteractionColumn(NumericMatrix IND, int p) {
  int k = IND.ncol();
  //NumericVector INDv=as<NumericVector>(IND);
  //NumericVector INDvi=floor(INDv);
  IntegerVector columnNumbers(k);

  for (int idx = 0; idx < k; ++idx) {
    int i = (IND(0, idx));
    int j = (IND(1, idx));

    // Ensure i <= j, as per the problem statement
    if (i > j) {
      std::swap(i, j);
    }

    // Calculate the column number
    //int column = ((i - 1) * (2 * p - i + 2)) / 2 + (j - i);

    int column;
    if(i==0){
      column=j+1;
    }
    else{
    column= (i - 1) * (2 * p - i) / 2 + j;
    }
    columnNumbers[idx] = column;
  }

  return columnNumbers;
}

// [[Rcpp::export]]
List pair_search4(NumericVector x, NumericVector y) {
  int n = x.size();
  // append elements from y to the end of x
  for (int i = 0; i<n; ++i) {
    x.push_back(y[i]);
  }
  //get size of x
  int N = x.size();
  //initiate list
  List list(0);
  IntegerVector sorted_indexes=sorted_index_vector(x);//(index_vec.begin(), index_vec.end());
  std::sort(x.begin(),x.end());
  int i=0;
  while(i<N){
    //initialize m to get split index
    int m=0;
    //start by checking consecutive elements
    int j=i+1;
    while(x[i]==x[j]){
      if(sorted_indexes[j]>=n&&sorted_indexes[j-1]<n){m=j;}//if consequent values come from different vectors, get the index to split
      j++;
    }
    Rcpp::List v(0);

    //if there are repetitions & they come from different vectors, split and push to different elements of list,
    //append list to main list

    if(((j-i)>1)&&m>0){
      v.push_back(sorted_indexes[Rcpp::Range(i,m-1)]+1);// indexes from first vector
      v.push_back((sorted_indexes[Rcpp::Range(m,j-1)])-n+1);//indexes from second vector
      list.push_back(v);
    }
    i=j;}// jump to next (non-repeated) value

  return list;}

//  perform uniform sampling from matrices X and Z, for the binary case
// [[Rcpp::export]]
List uniformSampling(NumericMatrix X, NumericVector Y, int p, int k) {
  int n= X.nrow(); // assuming the number of rows is equal to the size of Y
  int p1= X.ncol();
  NumericMatrix Z(n,p1);

  for (int i=0; i<n; ++i) {
    for (int l=0; l < p1; ++l) {
      Z(i,l)=Y[i]*X(i,l);}
  }

  List result; // to store results

  for (int j = 0; j < k; ++j) {
    IntegerVector sampledIndices=Rcpp::sample(n, p, false);
    std::sort(sampledIndices.begin(), sampledIndices.end());
    // initialize the result matrices for sampled rows
    NumericMatrix sampledX(p, X.ncol());
    NumericMatrix sampledZ(p, Z.ncol());

    // populate sampled submatrices using sampled indices
    for (int i = 0; i < p; ++i) {
      sampledX.row(i) = X.row(sampledIndices[i] - 1); // Adjust for 0-based indexing
      sampledZ.row(i) = Z.row(sampledIndices[i] - 1);
    }

    // append sampled submatrices to the result list
    result.push_back(pair_search4(as<NumericVector>(binaryToInt(sampledZ)),as<NumericVector>(binaryToInt(sampledX))));
  }

  return result;
}


// [[Rcpp::export]]
IntegerVector stl_sort(NumericVector x) {
  IntegerVector y= as<IntegerVector>(clone(x));
  std::sort(y.begin(), y.end());
  return y;
}

// [[Rcpp::export]]
List pairsearch11(IntegerVector a_positions, IntegerVector b_positions,
                  IntegerVector a_sorted_values, IntegerVector b_sorted_values) {
  int n= a_positions.size();
  int m= b_positions.size();
  List list(n);
  int a=0, b=0, z= 0;
  while (a < n && b < m) {
    double a_val= a_sorted_values[a];
    double b_val= b_sorted_values[b];

    if (a_val < b_val) {
      a++;
      continue;
    }

    if (a_val > b_val) {
      b++;
      continue;
    }

    int a_end= n;
    for (int i= a+1; i < n; i++) {
      if (a_sorted_values[i] != a_val) {
        a_end = i;
        break;
      }
    }

    int b_end = m;
    for (int i = b+ 1; i < m; i++) {
      if (b_sorted_values[i] != b_val) {
        b_end = i;
        break;
      }
    }
    if((a<=a_end-1) && (b<=b_end-1)){
      list[z]= expandGrid(a_positions[Range(a,a_end-1)],b_positions[Range(b,b_end-1)]);//(a_positions[Range(a_pos,a_end-1)]);
    }
    z++;

    a = a_end;
    b = b_end;
  }

  return list[Range(0, z- 1)];
}




//  perform weighted sampling from matrices X and Z
// [[Rcpp::export]]
NumericMatrix weightedSampling(NumericMatrix X, NumericVector Y, int p, int k) {
  int n = X.nrow(); // assuming the number of rows is equal to the size of Y
  int p1=X.ncol();
  NumericMatrix Z(n,p1);

  for (int i=0; i<n; ++i) {
    for (int l=0; l < p1;++l) {
      Z(i,l) = transformY(Y)[i]*X(i, l);
    }
  }

  // normalize weights to form a probability distribution
  NumericVector normalizedWeights = normalizeL1(Y);
  NumericVector luck(0);
  List result; // to store results

  for (int j = 0; j < k; ++j) {
    // generate p weighted random indexes from 1 to n
    IntegerVector sampledIndices = Rcpp::sample(n, p, false, normalizedWeights);

    // initialize the result matrices for sampled rows
    NumericMatrix sampledX(p, X.ncol());
    NumericMatrix sampledZ(p, Z.ncol());

    // populate sampled submatrices using sampled indices
    for (int i = 0; i < p; ++i) {
      sampledX.row(i) = X.row(sampledIndices[i] - 1); // Adjust for 0-based indexing
      sampledZ.row(i) = Z.row(sampledIndices[i] - 1);
    }

    // append sampled submatrices to the result list
    //result.push_back(as<NumericVector>(sampledZ));
    //result.push_back(pair_search4(as<NumericVector>(binaryToInt(sampledX)),as<NumericVector>(binaryToInt(sampledZ))));
    NumericVector a=as<NumericVector>(binaryToInt(sampledX));
    NumericVector b=as<NumericVector>(binaryToInt(sampledZ));
    List ps=pairsearch11(sorted_index_vector(a),sorted_index_vector(b), stl_sort(a),stl_sort(b));
    int s=ps.size();
    for(int i=0;i<s;++i){
      IntegerMatrix pr=(ps[i]);
      NumericVector pv=cantor_map(as<NumericMatrix>(pr));
      for(int j=0;j<pv.size();++j){
       luck.push_back(pv[i]);
       //luck.attr("dim")=Dimension(2,int(luck.length()/2));
       }

    }

    //result.push_back(pairsearch11 (sorted_index_vector(a),sorted_index_vector(b), stl_sort(a),stl_sort(b) ));
  }
  //luck.attr("dim")=Dimension(2,int(luck.length()/2));
  return inverse_cantor_map(luck);
  //return result;
}

//function to get unique pairs
// [[Rcpp::export]]
NumericMatrix getunique(NumericMatrix combined) {
  // convert the combined matrix to a vector
  NumericVector combinedVector = cantor_map(combined);

  // sort the combined vector
  std::sort(combinedVector.begin(), combinedVector.end());

  // remove duplicates
  NumericVector::iterator it = std::unique(combinedVector.begin(), combinedVector.end());
  combinedVector.erase(it, combinedVector.end());
  return inverse_cantor_map(combinedVector);
}


// [[Rcpp::export]]
IntegerVector nzeroindices(NumericVector x) {
  std::vector<int> indices;  // A vector to store the indices

  //loop through the input vector and find non-zero values
  for (int i = 0; i < x.size(); ++i) {
    if (x[i] != 0) {
      indices.push_back(i+1);  // R is 1-indexed
    }
  }

  return wrap(indices);  // Convert std::vector to Rcpp IntegerVector
}



//soft-thresholding operator
// [[Rcpp::export]]
double softThreshold(double z, double gamma) {
  if (z > 0 && gamma < std::abs(z)) {
    return (z - gamma);
  } else if (z < 0 && gamma < std::abs(z)) {
    return (z + gamma);
  } else {
    return 0;
  }
}

// [[Rcpp::export]]
NumericVector co(int v){
  NumericVector ty(v);
  return ty;
}


// [[Rcpp::export]]
List lassorisky(NumericVector Y, NumericMatrix X, double lambda, IntegerVector nzero_indices_beta, IntegerVector nzero_indices_theta, int max_iter = 1000, double tol = 1e-6) {
  int n = X.nrow();
  int p = X.ncol();
  int num_interactions = p*(p + 1) / 2;
  NumericVector beta(p);
  NumericVector theta(num_interactions);
  NumericVector beta_old(p);
  NumericVector theta_old(num_interactions);
  int nzerobet=nzero_indices_beta.size();
  int nzerothet=nzero_indices_theta.size();

//  for(int i=0;i<zerobet;++i){
//    beta[zero_indices_beta[i]-1]=0;
    //theta[zero_indices_theta[i]-1]=0;
//  }

//  for(int i=0;i<zerothet;++i){
//    //beta[zero_indices_beta[i]-1]=0;
//    theta[zero_indices_theta[i]-1]=0;
//  }


  for (int iter = 0; iter < max_iter; ++iter) {
    std::copy(beta.begin(), beta.end(), beta_old.begin());
    std::copy(theta.begin(), theta.end(), theta_old.begin());

    //update beta
    for (int b = 0; b < nzerobet; ++b) {
      int j=nzero_indices_beta[b]-1;
      double X_jY = 0;
      double X_jX_j = 0;

      for (int i=0; i<n; ++i) {
        double r_ij = Y[i];
        for (int k=0; k<p; ++k) {
          if (k != j && beta[k]!=0) {
            r_ij -= X(i, k) * beta[k];
          }
        }
        //interaction terms
        int index = 0;
        for (int k = 0; k < p; ++k) {
          for (int l = k; l < p; ++l) {

            if (theta[index]!=0) {
              r_ij -= X(i, k) * X(i, l) * theta[index];
            }
            index++;
          }
        }
        if(r_ij!=0){
        X_jY += X(i, j)*r_ij;
        X_jX_j += X(i, j) * X(i, j);
        }
      }

      beta[j] = softThreshold(X_jY / X_jX_j, lambda / X_jX_j);

    }
    //for(int i=0;i<zerobet;++i){
      //beta[zero_indices_beta[i]-1]=0;
      //theta[zero_indices_theta[i]-1]=0;
    //}

    //update theta
    int index = 0;
    for (int k = 0; k < p; ++k) {
      for (int l = k; l < p; ++l) {

        double W_klY = 0;
        double W_klW_kl = 0;

        for (int i = 0; i < n; ++i) {
          double r_il = Y[i];
          for (int m = 0; m < p; ++m) {
            if (beta[m]!=0) {
              r_il -= X(i, m)*beta[m];
            }
          }
          //interaction terms
          int inner_index = 0;
          for (int m = 0; m < p; ++m){
            for (int n = m; n < p; ++n){
              if (inner_index != index && theta[inner_index]!=0){
                r_il -= X(i, m)*X(i, n) *theta[inner_index];
              }
              inner_index++;
            }
          }
          if(r_il!=0){
          W_klY += X(i, k) * X(i, l) * r_il;
          W_klW_kl += X(i, k) * X(i, l) * X(i, k) * X(i, l);
          }
        }

        theta[index] = softThreshold(W_klY / W_klW_kl, lambda / W_klW_kl);
        index++;
      }
    }
    //for(int i=0;i<nzerothet;++i){
      //beta[zero_indices_beta[i]-1]=0;
     // theta[nzero_indices_theta[i]-1]=0;
    //}


    double max_diff = 0;
    for (int j = 0; j < p; ++j) {
      max_diff = std::max(max_diff, std::abs(beta[j] - beta_old[j]));
    }
    for (int j = 0; j < num_interactions; ++j) {
      max_diff = std::max(max_diff, std::abs(theta[j] - theta_old[j]));
    }

    if (max_diff < tol) {
      break;
    }
  }

  return List::create(Named("beta") = beta, Named("theta") = theta);
}

// [[Rcpp::export]]
List lassorisky2(NumericVector Y, NumericMatrix X, double lambda, IntegerVector nzero_indices_beta, IntegerVector nzero_indices_theta, int max_iter = 1000, double tol = 1e-6) {
  int n = X.nrow();
  int p = X.ncol();
  int num_interactions = p * (p + 1) / 2;
  NumericVector beta(p);
  NumericVector theta(num_interactions);
  NumericVector beta_old(p);
  NumericVector theta_old(num_interactions);
  int nzerobet = nzero_indices_beta.size();
  int nzerothet = nzero_indices_theta.size();

  for (int iter = 0; iter < max_iter; ++iter) {
    std::copy(beta.begin(), beta.end(), beta_old.begin());
    std::copy(theta.begin(), theta.end(), theta_old.begin());

    NumericVector residual(Y);

    // Subtract the contribution of beta coefficients
    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < p; ++j) {
        if (beta[j] != 0) {
          residual[i] -= X(i, j) * beta[j];
        }
      }
    }

    // Subtract the contribution of theta coefficients
    int index = 0;
    for (int k = 0; k < p; ++k) {
      for (int l = k; l < p; ++l) {
        if (theta[index] != 0) {
          for (int i = 0; i < n; ++i) {
            residual[i] -= X(i, k) * X(i, l) * theta[index];
          }
        }
        index++;
      }
    }

    // Update beta
    for (int b = 0; b < nzerobet; ++b) {
      int j = nzero_indices_beta[b] - 1;
      double X_jY = 0;
      double X_jX_j = 0;

      for (int i = 0; i < n; ++i) {
        double r_ij = residual[i] + X(i, j) * beta[j];
        X_jY += X(i, j) * r_ij;
        X_jX_j += X(i, j) * X(i, j);
      }

      beta[j] = softThreshold(X_jY / X_jX_j, lambda / X_jX_j);
    }

    // Update theta
    index = 0;
    for (int k = 0; k < p; ++k) {
      for (int l = k; l < p; ++l) {
        double W_klY = 0;
        double W_klW_kl = 0;

        for (int i = 0; i < n; ++i) {
          double r_il = residual[i] + X(i, k) * X(i, l) * theta[index];
          W_klY += X(i, k) * X(i, l) * r_il;
          W_klW_kl += X(i, k) * X(i, l) * X(i, k) * X(i, l);
        }

        theta[index] = softThreshold(W_klY / W_klW_kl, lambda / W_klW_kl);
        index++;
      }
    }

    // Check for convergence
    double max_diff = 0;
    for (int j = 0; j < p; ++j) {
      max_diff = std::max(max_diff, std::abs(beta[j] - beta_old[j]));
    }
    for (int j = 0; j < num_interactions; ++j) {
      max_diff = std::max(max_diff, std::abs(theta[j] - theta_old[j]));
    }

    if (max_diff < tol) {
      break;
    }
  }

  return List::create(Named("beta") = beta, Named("theta") = theta);
}

// [[Rcpp::export]]
List lassoInteractions(NumericVector Y, NumericMatrix X, double lambda, IntegerVector zero_indices_beta, IntegerVector zero_indices_theta, int max_iter = 1000, double tol = 1e-6) {
  int n = X.nrow();
  int p = X.ncol();
  int num_interactions = p * (p + 1) / 2;
  NumericVector beta(p);
  NumericVector theta(num_interactions);
  NumericVector beta_old(p);
  NumericVector theta_old(num_interactions);
  int zerobet=zero_indices_beta.size();
  int zerothet=zero_indices_theta.size();

  //  for(int i=0;i<zerobet;++i){
  //    beta[zero_indices_beta[i]-1]=0;
  //theta[zero_indices_theta[i]-1]=0;
  //  }

  //  for(int i=0;i<zerothet;++i){
  //    //beta[zero_indices_beta[i]-1]=0;
  //    theta[zero_indices_theta[i]-1]=0;
  //  }


  for (int iter = 0; iter < max_iter; ++iter) {
    std::copy(beta.begin(), beta.end(), beta_old.begin());
    std::copy(theta.begin(), theta.end(), theta_old.begin());

    // Update beta
    for (int j = 0; j < p; ++j) {

      double X_jY = 0;
      double X_jX_j = 0;

      for (int i = 0; i < n; ++i) {
        double r_ij = Y[i];
        for (int k = 0; k < p; ++k) {
          if (k != j && beta[k]!=0) {
            r_ij -= X(i, k) * beta[k];
          }
        }
        //interaction terms
        int index = 0;
        for (int k = 0; k < p; ++k) {
          for (int l = k; l < p; ++l) {
            if (theta[index]!=0) {
              r_ij -= X(i, k) * X(i, l) * theta[index];
            }
            index++;
          }
        }
        if(r_ij!=0){
          X_jY += X(i, j) * r_ij;
          X_jX_j += X(i, j) * X(i, j);
        }
      }

      beta[j] = softThreshold(X_jY / X_jX_j, lambda / X_jX_j);

    }
    //for(int i=0;i<zerobet;++i){
      //beta[zero_indices_beta[i]-1]=0;
      //theta[zero_indices_theta[i]-1]=0;
    //}

    //update theta
    int index = 0;
    for (int k = 0; k < p; ++k) {
      for (int l = k; l < p; ++l) {

        double W_klY = 0;
        double W_klW_kl = 0;

        for (int i = 0; i < n; ++i) {
          double r_il = Y[i];
          for (int m = 0; m < p; ++m) {
            if (beta[m]!=0) {
              r_il -= X(i, m)*beta[m];
            }
          }
          // Interaction terms
          int inner_index = 0;
          for (int m = 0; m < p; ++m){
            for (int n = m; n < p; ++n){
              if (inner_index != index && theta[inner_index]!=0){
                r_il -= X(i, m) * X(i, n) * theta[inner_index];
              }
              inner_index++;
            }
          }
          if(r_il!=0){
            W_klY += X(i, k) * X(i, l) * r_il;
            W_klW_kl += X(i, k) * X(i, l) * X(i, k) * X(i, l);
          }
        }

        theta[index] = softThreshold(W_klY / W_klW_kl, lambda / W_klW_kl);
        index++;
      }
    }
    //for(int i=0;i<zerothet;++i){
      //beta[zero_indices_beta[i]-1]=0;
    //  theta[zero_indices_theta[i]-1]=0;
    //}


    double max_diff = 0;
    for (int j = 0; j < p; ++j) {
      max_diff = std::max(max_diff, std::abs(beta[j] - beta_old[j]));
    }
    for (int j = 0; j < num_interactions; ++j) {
      max_diff = std::max(max_diff, std::abs(theta[j] - theta_old[j]));
    }

    if (max_diff < tol) {
      break;
    }
  }

  return List::create(Named("beta") = beta, Named("theta") = theta);
}

NumericMatrix createInteractionMatrix(NumericMatrix X, IntegerVector indices) {
  int n = X.nrow();
  int p = X.ncol();
  int k = indices.size();
  NumericMatrix interactionMatrix(n, k);

  for (int idx = 0; idx < k; ++idx) {
    int interactionIndex = indices[idx];

    // Find the column indices (i, j) for the interactionIndex
    int i = 0, j = 0, count = 0;
    bool found = false;
    for (i = 0; i < p; ++i) {
      for (j = i; j < p; ++j) {
        if (count == interactionIndex) {
          found = true;
          break;
        }
        count++;
      }
      if (found) break;
    }

    // Fill the interaction matrix with the product of the respective columns
    for (int row = 0; row < n; ++row) {
      interactionMatrix(row, idx) = X(row, i) * X(row, j);
    }
  }

  return interactionMatrix;
}

// [[Rcpp::export]]
List computesolution(NumericVector Y, NumericMatrix X, IntegerVector nzero_indices_beta, IntegerVector nzero_indices_theta, double lambda, int max_iter = 1000, double tol = 1e-6) {
  int n = X.nrow();
  int p = X.ncol();
  int num_interactions = p*(p+1)/2;
  NumericVector beta(p);
  NumericVector theta(num_interactions);
  NumericVector beta_old(p);
  NumericVector theta_old(num_interactions);
  int nzerobet = nzero_indices_beta.size();
  int nzerothet = nzero_indices_theta.size();
  double epsilon=1e-10;

  for (int iter = 0; iter < max_iter; ++iter) {
    std::copy(beta.begin(), beta.end(), beta_old.begin());
    std::copy(theta.begin(), theta.end(), theta_old.begin());

    NumericVector residual = clone(Y);

    // Subtract the contribution of beta coefficients
    for (int b = 0; b < nzerobet; ++b) {
      int j = nzero_indices_beta[b] - 1;
      if (beta[j] != 0) {
        for (int i = 0; i < n; ++i) {
          residual[i] -= X(i, j) * beta[j];
        }
      }
    }

    for (int t = 0; t < nzerothet; ++t) {
      int index = nzero_indices_theta[t]-1;

      // Find the column indices (i, j) for the interactionIndex
      int k = 0, l = 0, count = 0;
      bool found = false;
      for (k = 0; k < p; ++k) {
        for (l = k; l < p; ++l) {
          if (count == index) {
            found = true;
            break;
          }
          count++;
        }
        if (found) break;
      }


      if (theta[index] != 0) {
        for (int i = 0; i < n; ++i) {
          residual[i] -= X(i, k) * X(i, l) * theta[index];
        }
      }
    }

    // Update beta
    for (int b = 0; b < nzerobet; ++b) {
      int j = nzero_indices_beta[b] - 1;
      double X_jY = 0;
      double X_jX_j = 0;

      for (int i = 0; i < n; ++i) {
        double r_ij=residual[i] + X(i, j)*beta[j];
        if(r_ij!=0){
        X_jY += X(i, j) * r_ij;
        X_jX_j += X(i, j) * X(i, j);
        }
      }
      if(X_jX_j!=0){
      beta[j] = softThreshold(X_jY /X_jX_j, lambda /X_jX_j);
      }
      }

    // Update theta
    for (int t = 0; t < nzerothet; ++t) {
      int index = nzero_indices_theta[t]-1;

      // Find the column indices (i, j) for the interactionIndex
      int k = 0, l = 0, count = 0;
      bool found = false;
      for (k = 0; k < p; ++k) {
        for (l = k; l < p; ++l) {
          if (count == index) {
            found = true;
            break;
          }
          count++;
        }
        if (found) break;
      }

      double W_klY = 0;
      double W_klW_kl = 0;

      for (int i = 0; i < n; ++i) {
        double r_il = residual[i] + X(i, k) * X(i, l) * theta[index];
        if(r_il!=0){
        W_klY += X(i, k) * X(i, l) * r_il;
        W_klW_kl += X(i, k) * X(i, l) * X(i, k) * X(i, l);
        }
      }
      if(W_klW_kl!=0){
      theta[index] = softThreshold(W_klY / (W_klW_kl+epsilon), lambda / (W_klW_kl+epsilon));
      }
      }

    // Check for convergence
    double max_diff = 0;
    for (int j = 0; j < p; ++j) {
      max_diff = std::max(max_diff, std::abs(beta[j] - beta_old[j]));
    }
    for (int j = 0; j < num_interactions; ++j) {
      max_diff = std::max(max_diff, std::abs(theta[j] - theta_old[j]));
    }

    if (max_diff < tol) {
      break;
    }
  }

  return List::create(Named("beta") = beta, Named("theta") = theta);
}

// [[Rcpp::export]]
IntegerVector ck(IntegerMatrix s){
  return as<IntegerVector>(s);
}


/***R
# Example usage in R
#set.seed(123)
n <- 100
p <- 5
X <- matrix(sample(1:100, n * p, replace = TRUE), n, p)

#<-normalizeMatrix(X)
beta_true <- c(0, 1, 0, 2, 0)
theta_true <- c(0, 1,6,8, 99,88,61,43,8,0,0,80,48,0,0)
Y <- X %*% beta_true + generate_interaction_matrix(X)%*%theta_true #+ rnorm(n)
lambda <- 0.1
zero_indices_beta <- c(2,4)                          #R is 1-indexed, but C++ is 0-indexed
zero_indices_theta <- c(2,3,4,5,6, 7,8, 9, 12, 13)   #R is 1-indexed, but C++ is 0-indexed

#//result <- lassoInteractions(Y, X, lambda, zero_indices_beta, zero_indices_theta)
#//print(result$beta)
#//print(result$theta)
*/
//

// [[Rcpp::export]]
NumericMatrix normalizeMatrix(NumericMatrix X) {
  int n = X.nrow();
  int p = X.ncol();

  NumericMatrix X_normalized(n, p);

  for (int j = 0; j < p; ++j) {
    NumericVector col = X(_, j);
    double mean_col = mean(col);
    double sd_col = sd(col);

    for (int i = 0; i < n; ++i) {
      if (sd_col != 0) {
        X_normalized(i, j) = (X(i, j) - mean_col) / sd_col;
      } else {
        X_normalized(i, j) = 0; // If sd is 0, all elements are the same, hence normalized to 0
      }
    }
  }

  return X_normalized;
}

// [[Rcpp::export]]
NumericVector normalizeVector(NumericVector Y) {
  double mean_Y = mean(Y);
  double sd_Y = sd(Y);

  NumericVector Y_normalized(Y.size());

  for (int i = 0; i < Y.size(); ++i) {
    if (sd_Y != 0) {
      Y_normalized[i] = (Y[i] - mean_Y) / sd_Y;
    } else {
      Y_normalized[i] = 0; // If sd is 0, all elements are the same, hence normalized to 0
    }
  }

  return Y_normalized;
}

// [[Rcpp::export]]
int size(NumericVector lm){
  return lm.size();
}

// [[Rcpp::export]]
List transform_pairs(List nested_pairs) {
  int n = nested_pairs.size();
  List result(n);

  for (int i = 0; i < n; ++i) {
    List pair = nested_pairs[i];
    IntegerVector vec(2);
    vec[0] = as<IntegerVector>(pair[0])[0];
    vec[1] = as<IntegerVector>(pair[1])[0];
    result[i] = vec;
  }

  return result;
}

// [[Rcpp::export]]
IntegerVector tecxt(NumericVector X, NumericVector Y){
  List eqpairs=transform_pairs(pair_search4(Y, X));
  return as<IntegerVector>(eqpairs[1]);

}

// [[Rcpp::export]]
List computesolutionxyz(NumericMatrix X, NumericVector Y, NumericVector lambda_grid) {
  int L = lambda_grid.size();
  int n = X.nrow();
  int p = X.ncol();

  List lasso_solutions(L);

  IntegerVector A(1);
  IntegerVector B(1);

  for (int l = 0; l < L; ++l) {
    double lambda = lambda_grid[l];

    // Compute Lasso solution
    List solution = computesolution(Y, X, A, B, lambda);
    NumericVector beta = solution["beta"];
    NumericVector theta = solution["theta"];

    // Check KKT conditions
    IntegerVector U;
    IntegerVector V;

    // Compute residuals
    NumericVector residual = clone(Y);
    for (int j = 0; j < A.size(); ++j) {
      //int col = A[j] - 1;  // Convert 1-based index to 0-based
      //double beta_j = beta[j];
      for (int i = 0; i < n; ++i) {
        residual[i] -= X(i, A[j] - 1) * beta[j];
      }
    }

    int interaction_index = 0;
    int B_index = 0;

    for (int k = 0; k < p; ++k) {
      for (int l = k; l < p; ++l) {
        // Check if the current interaction index is in B
        if (B_index < B.size() && B[B_index] == interaction_index + 1) {  // +1 to convert to 1-based index
          //double theta_kl = theta[B_index];
          for (int i = 0; i < n; ++i) {
            residual[i] -= X(i, k) * X(i, l) * theta[B_index];
          }
          B_index++;
        }
        interaction_index++;
      }
    }

    // Update U and V based on KKT conditions
    for (int k = 0; k < p; ++k) {
      double XT_k_residual = sum(X(_, k) * residual) / n;
      if (std::abs(XT_k_residual) > lambda) {
        U.push_back(k + 1);
      }
    }

    //incorporating lasso
    List eqpairs=transform_pairs(pair_search4(residual, X));
    IntegerVector eqpairs1=getInteractionColumn(getunique(weightedSampling(X,residual,3,3)), p);
       //getunique(weightedSampling(X,residual,3,3));

    // Interaction terms
    int num_interactions = p * (p + 1) / 2;
    for(int i=0; i< eqpairs.size();++i){
      V.push_back(i);
    }
    //for (int k = 0; k < p; ++k) {

      //for (int l = k; l < p; ++l) {

        //this is not computationally efficient
        //what are you thinking
        //IntegerVector pair=as<IntegerVector>(eqpairs[pairc]);
        //if(k==pair[1] && l==pair[2]){
          //;
        //}
        //pairc++;
        //if(pairc==eqpairs.size()){
        //  exin=true;
        //  break;
        //}
        //double W_kl_residual = sum(X(_, k) * X(_, l) * residual) / n;
        //if (std::abs(W_kl_residual) > lambda) {
        //  V.push_back(count + 1);
        //}
        //ount++;
      //}
      //if(exin){
      //  break;
      //}
    //}

    // If U and V are empty, stop updating A and B
    if (U.size() == 0 && V.size() == 0) {
      lasso_solutions[l] = List::create(Named("beta") = beta, Named("theta") = theta);
      break;
    } else {
      // Update A and B
      for (int u = 0; u < U.size(); ++u) {
        if (std::find(A.begin(), A.end(), U[u]) == A.end()) {
          A.push_back(U[u]);
        }
      }
      for (int v = 0; v < V.size(); ++v) {
        if (std::find(B.begin(), B.end(), V[v]) == B.end()) {
          B.push_back(V[v]);
        }
      }
    }

    lasso_solutions[l] = List::create(Named("beta") = beta, Named("theta") = theta);
  }

  return lasso_solutions;
}



///*** R
//# Example usage in R
//X <- matrix(c(1, 2, 3, 4, 5, 6), nrow=3, ncol=2)
//Y <- c(1, 2, 3, 4, 5)

//X_normalized <- normalizeMatrix(X)
//Y_normalized <- normalizeVector(Y)

//print(X_normalized)
//print(Y_normalized)
//*/

// [[Rcpp::export]]
NumericMatrix generate_interaction_matrix(NumericMatrix X) {
  int n = X.nrow();
  int p = X.ncol();
  int num_interactions = p * (p + 1) / 2;

  // Initialize the interaction matrix W
  NumericMatrix W(n, num_interactions);

  int col_idx = 0;

  for (int i = 0; i < p; ++i) {
    for (int j = i; j < p; ++j) {
      for (int k = 0; k < n; ++k) {
        W(k, col_idx) = X(k, i) * X(k, j);
      }
      col_idx++;
    }
  }

  return W;
}


// [[Rcpp::export]]
NumericVector hadamard(NumericVector x, NumericVector y) {
  int n = x.size();
  NumericVector result(n);

  for (int i= 0; i< n; i++) {
    result[i]=x[i]*y[i];
  }

  return result;
}

// function to compute the inner product of two vectors
// [[Rcpp::export]]
double inner_product(NumericVector x, NumericVector y) {
  int n = x.size();
  double result = 0.0;

  for (int i= 0; i< n; i++) {
    result+= x[i]*y[i];
  }

  return result;
}

//filter out pairs below a certain threshold
// [[Rcpp::export]]
NumericMatrix filter(NumericMatrix pairs, NumericMatrix X, NumericVector Y, double threshold){
  int p= pairs.ncol();
  int c=0;
  NumericVector filtered(0);
  for(int i=0; i<p; i++){
    if(inner_product(Y,hadamard(X(_,pairs(0,i)-1), X(_,pairs(1,i)-1))) >= threshold){
    ++c;
    filtered.push_back(pairs(0,i));
    filtered.push_back(pairs(1,i));
    }
  }
  filtered.attr("dim") = Dimension(2, int(filtered.size()/2));
  return as<NumericMatrix>(filtered);
}

// [[Rcpp::export]]
NumericVector generate_uniform_values(int n, double a = 0.0, double b = 1.0) {
  NumericVector result(n);

  // generate random values from U[a, b]
  for (int i = 0; i < n; ++i) {
    result[i] = R::runif(a, b);
  }

  return result;
}



// [[Rcpp::export]]
NumericVector generate_random_projection(int n, int M, bool with_replacement) {
  NumericVector D = Rcpp::runif(M); // generate a vector D with independent components from U[0, 1]

  NumericVector R(n); // initialize vector R with zeros
  NumericVector indices1(M);
  IntegerVector v = seq(1,n);
  // sampling
  indices1 = Rcpp::sample(v, M, with_replacement);

  for (int i = 0; i < M; ++i) {
    indices1[i] = indices1[i] - 1;
  }

  for (int i = 0; i < n; ++i) {
    IntegerVector ind = findIndex(indices1, i);
    float sum1 = 0.0; // declare and initialize sum1
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


//basic binary search function
// [[Rcpp::export]]
bool binary_search_cpp(NumericVector arr, double target) {
  std::sort(arr.begin(), arr.end());
  int left=0;
  int right=arr.size() - 1;

  while (left <= right) {
    int mid = left + (right - left) / 2;
    if (arr[mid]==target) {
      return true;
    } else if (arr[mid] < target) {
      left=mid + 1;
    } else {
      right=mid - 1;
    }
  }

  return false;
}



// function to calculate the interaction strength gammajk for pair (j, k)
// [[Rcpp::export]]
double interaction_strength(NumericMatrix X, NumericVector Y, int j, int k) {
  int n = X.nrow();
  int p = X.ncol();
  double sum = 0.0;

  // calculate Z(n x p) matrix
  NumericMatrix Z(n, p);
  for (int i=0; i<n; ++i) {
    for (int l=0; l < p;++l) {
      Z(i,l)=Y[i]*X(i,l);
    }
  }

  // calculate the interaction strength gamma_jk
  for (int i= 0; i< n; ++i) {
    // check if Yi = Xij * Xik
    if (Y[i]== X(i, j)*X(i, k)) {
      sum+= 1.0;
    }
  }

  // retrn it
  return sum / n;
}


// function to find pair matches between vectors x and z
// [[Rcpp::export]]
NumericMatrix find_pair_matches(IntegerVector x0,IntegerVector z0) {
  int p = x0.size();
  int n = z0.size();
  NumericMatrix result(2, p*p);

  NumericVector x= as<NumericVector>(x0);
  NumericVector z= as<NumericVector>(z0);

  // convert to use std lib
  std::vector<double> new_vec_1(x.begin(), x.end());
  std::vector<double> new_vec_2(z.begin(), z.end());

  //when sorting vector, get how index is sorted
  IntegerVector index_vector=sorted_index_vector(wrap(new_vec_1));

  //sort new_vec_1, create temporary vec
  std::sort(new_vec_1.begin(), new_vec_1.end());
  std::vector<double> temp=new_vec_1;

  // find pair matches using binary search and store the indices
  int col = 0;
  for (int i = 0; i < n; ++i){
    new_vec_1=temp;
    while(std::binary_search(new_vec_1.begin(), new_vec_1.end(), new_vec_2[i])){
      int d=std::distance(new_vec_1.begin(), std::lower_bound(new_vec_1.begin(), new_vec_1.end(), new_vec_2[i]));
      result(0, col) = index_vector[d] + 1;
      result(1, col) = i + 1;
      new_vec_1[d]-=0.001; //so that the iterator will skip this (as we already recorded the index) and still count it (diguising value)
      col++;
    }

  }

  if(col>0){return result(_, Range(0, col - 1));} //cut unused columns

  else{NumericMatrix nothing(2, 1); return nothing;} //no matches
}

// function to find pair matches between vectors x and z
// [[Rcpp::export]]
NumericMatrix find_pair_matches2(IntegerVector x0,IntegerVector z0) {
  int p = x0.size();
  int n = z0.size();
  NumericMatrix result(2, p*p);

  NumericVector x = as<NumericVector>(x0);
  NumericVector z = as<NumericVector>(z0);

  // convert to use std lib
  std::vector<double> new_vec_1(x.begin(), x.end());
  std::vector<double> new_vec_2(z.begin(), z.end());

  //when sorting vector, get how the index is sorted
  IntegerVector index_vector=sorted_index_vector(wrap(new_vec_1));

  //sort new_vec_1, create temporary vec
  std::sort(new_vec_1.begin(), new_vec_1.end());
  std::vector<double> temp=new_vec_1;

  // find pair matches using binary search and store the indices
  int col = 0;
  for (int i = 0; i < n; ++i){
    new_vec_1=temp;
    while(std::binary_search(new_vec_1.begin(), new_vec_1.end(), new_vec_2[i])){
      int d=std::distance(new_vec_1.begin(), std::lower_bound(new_vec_1.begin(), new_vec_1.end(), new_vec_2[i]));
      result(0, col) = index_vector[d] + 1;
      result(1, col) = i + 1;
      new_vec_1[d]-=0.001; //so that the iterator will skip this (as we already recorded the index) and still count it (diguising value)
      col++;
      //new_vec_1=cut_first_element(new_vec_1);
    }

  }

  if(col>0){return result(_, Range(0, col - 1));} //cut unused columns

  else{NumericMatrix nothing(2, 1); return nothing;} //no matches
}

// function implementing final xyz (theoretical)
// [[Rcpp::export]]
IntegerMatrix strongest_pairs(NumericMatrix X, NumericVector Y, int M, int L, int gamma) {
  int n= X.nrow();
  int p= X.ncol();
  NumericMatrix Z(n,p);
  if(n!=Y.size()){
    stop("Inconsistency in Dimensions, come back later");
  }
  for (int i=0; i<n; ++i) {
    for (int l=0; l < p;++l) {
      Z(i,l)= Y[i]*X(i, l);
    }
  }
  IntegerMatrix I(2,0);
  for (int i=0;i<L;++i){
    NumericVector R=generate_random_projection(n, M, 0);
    NumericVector x=transpose(X)*R;
    NumericVector z=transpose(Z)*R;
    IntegerMatrix eq(2,1);
    for(int i=0;i < eq.ncol();++i){

      if(interaction_strength(X,Y,eq(_,i)[0],eq(_,i)[1])>gamma){
      i++;//push(I,eq(_,i));
      }

    }
  }

  return I;


}


// function implementing final optimized xyz: given binary matrix X and bin vector Y
//it returns all matching column pairs in X and Z, where Zij=Yi*Xij
// [[Rcpp::export]]
NumericMatrix strongest_pairs_binary(NumericMatrix X, NumericVector Y) {
  int n = X.nrow();
  int p = X.ncol();
  NumericMatrix Z(n,p);
  if(n!=Y.size()){
    stop("Inconsistency in Dimensions, come back later when you attain consistency");
  }
  for (int i=0; i<n; ++i) {
    for (int l=0; l < p;++l) {
      Z(i,l) = Y[i]*X(i, l);
    }
  }
  return find_pair_matches(binaryToInt(X),binaryToInt(Z));
}

// Function that cuts everything it sees
// [[Rcpp::export]]
std::vector<double> cut(const std::vector<double>& input_vec) {
  return std::vector<double>(input_vec.begin(), input_vec.end()-1);
}


// [[Rcpp::export]]
NumericMatrix generateMatrix(int n, int p) {
  NumericMatrix matrix(n, p);
  return matrix;
}

// function to create a random binary matrix, to edit 0 after ? to replace w/ -1
// [[Rcpp::export]]
NumericMatrix random_binary_matrix(int n, int p) {
  NumericMatrix mat(n, p);


  // Fill the matrix with random binary values
  for (int i= 0; i< n; ++i) {
    for (int j= 0; j< p; ++j) {
      // making a random value between 0 and 1
      double random_value= R::runif(0, 1);

      // convert to binary (0/1)
      mat(i,j) = (random_value < 0.5) ? -1 : 1;
    }
  }

  return mat;
}

// function to generate a random binary vector of size n with elements 0 and 1
// [[Rcpp::export]]
NumericVector random_binary_vector(int n) {
  NumericVector binaryVector(n);

  // seed for reproducibility
  srand(time(0));

  for (int i= 0; i< n; i++) {
    int randomNumber= rand() % 2; // generating 0/1
    binaryVector[i]= (randomNumber== 0) ? -1 : 1;
  }

  return binaryVector;
}


// [[Rcpp::export]]
NumericVector go(int n, int p) {
  NumericVector vec(n); // Create a NumericVector with size n
  std::fill(vec.begin(), vec.end(), p); // Set all elements to the value p
  return vec;
}


// [[Rcpp::export]]
NumericVector r(int n, int minValue=0, int maxValue=10) {
  NumericVector result(n);

  for (int i = 0; i < n; ++i) {
    result[i] = R::runif(minValue, maxValue + 1); // +1 because runif generates numbers in [a, b)
  }

  return result;
}

// [[Rcpp::export]]
IntegerVector rs(int n, int minValue=0, int maxValue=100) {
  IntegerVector result(n);

  for (int i = 0; i < n; ++i) {
    result[i] = R::runif(minValue, maxValue + 1); // +1 because runif generates numbers in [a, b)
  }

  return result;
}


//[[Rcpp::export]]
IntegerMatrix makeZ(IntegerMatrix X, IntegerVector Y){
 int n = X.nrow();
 int p = X.ncol();
 IntegerMatrix Z(n,p);
 if(n!=Y.size()){
   stop("Inconsistency in Dimensions, come back later when you attain consistency");
  }
 for (int i=0; i<n; ++i) {
  for (int l=0; l < p;++l) {
    Z(i,l) = Y[i]*X(i, l);
  }
 }
 return Z;}

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
        //add pairs that are clsoe
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


//[[Rcpp::export]]
IntegerMatrix equalpairs2(NumericVector u, NumericVector v, IntegerVector ou, IntegerVector ov, int max_number_of_pairs) {
  //set sizes of array
  int nu = u.size();
  int nv = v.size();

  //init two lists to store pairs
  std::list<int> pairs_u;
  std::list<int> pairs_v;

  //set pointers
  int start = 0;
  int j = 0;
  int i=0;
  //set counter
  int count = 0;

  //set precision epsilon

  if(v[0]<u[0]){
    ++start;
  }

  //start looping through u vector
  while(i < nu) {

    //increase if too small
    while(v[start]<u[i] && start < nv-1) {
      ++start;
    }

    //if close consider the pairs that might be close
      while(v[j]==u[i]) {
        //add pairs that are clsoe
        pairs_u.push_front(ou[i]);
        pairs_v.push_front(ov[j]);
        ++j;
        ++count;
        if(j >= nv) {
          break;
        }
      }
      if(u[i]==u[i+1]){
        ++i;
      }
    //++i;
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

// [[Rcpp::export]]
IntegerMatrix expandGrido(IntegerVector vec1, IntegerVector vec2) {
  int n= vec1.size();
  int m= vec2.size();

  IntegerMatrix result(2,n* m);

  for (int i= 0; i< n; ++i) {
    for (int j= 0; j< m; ++j) {
      result(0, i * m +j) = vec1[i];
      result(1, i * m +j) = vec2[j];
    }
  }

  return result;
}




// [[Rcpp::export]]
List pairsearch10(IntegerVector a_positions, IntegerVector b_positions,
                                    IntegerVector a_sorted_values, IntegerVector b_sorted_values) {
  int n= a_positions.size();
  int m= b_positions.size();
  List list(n);
  int a=0, b=0, z= 0;
  while (a < n && b < m) {
    double a_val= a_sorted_values[a];
    double b_val= b_sorted_values[b];

    if (a_val < b_val) {
      a++;
      continue;
    }

    if (a_val > b_val) {
      b++;
      continue;
    }

    int a_end= n;
    for (int i= a+1; i < n; i++) {
      if (a_sorted_values[i] != a_val) {
        a_end = i;
        break;
      }
    }

    int b_end = m;
    for (int i = b+ 1; i < m; i++) {
      if (b_sorted_values[i] != b_val) {
        b_end = i;
        break;
      }
    }
    if((a<=a_end-1) && (b<=b_end-1)){
    list[z]=expandGrido(a_positions[Range(a,a_end-1)],b_positions[Range(b,b_end-1)]);//(a_positions[Range(a_pos,a_end-1)]);
    }
    z++;

    a = a_end;
    b = b_end;
  }

  return list[Range(0, z- 1)];
}




