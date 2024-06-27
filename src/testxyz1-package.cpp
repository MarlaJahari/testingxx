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
    double w= floor((-1 + sqrt(1+ 8*values[i]))/ 2);
    double t= (w*w +w)/2;//values[i] - 0.5 *t*(t + 1);


    double y=values[i]-t;


    //double x= t-w;
    double x= w-y;

    result(0,i)=round(x);
    result(1,i)=round(y);

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
    else {  vec[i]= 1;}
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

    //ensure i <= j, as is always the case
    if (i > j) {
      std::swap(i, j);
    }

    //calculating the column number
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
NumericMatrix getInteractionIndices(IntegerVector columns, int p) {
  int k = columns.size();
  NumericMatrix indices(2, k);

  for (int idx = 0; idx < k; ++idx) {
    int column = columns[idx];

    //reverse the process to get i and j
    int i = 0;
    while (column >= (i * (2 * p - i) / 2)) {
      ++i;
    }
    --i;

    int j = column - (i * (2 * p - i) / 2);

    //store indices in the result matrix
    indices(0, idx) = i;
    indices(1, idx) = j;
  }

  return indices;
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
  List ans;
  if(z>=1){
  ans=list[Range(0, z- 1)];
  }
  else{
  ans=List();
  }
  return ans;}

//function to get unique pairs
// [[Rcpp::export]]
NumericMatrix getunique(NumericMatrix combined) {
    // convert the combined matrix to a vector
  NumericVector combinedVector = cantor_map(combined);

    //sort the combined vector
  std::sort(combinedVector.begin(), combinedVector.end());

    //remove duplicates
  NumericVector::iterator it = std::unique(combinedVector.begin(), combinedVector.end());
  combinedVector.erase(it, combinedVector.end());
  return inverse_cantor_map(combinedVector);
}



//perform uniform sampling from matrices X and Z, for the binary case
// [[Rcpp::export]]
NumericMatrix uniformSampling(NumericMatrix X, NumericVector Y, int p, int k) {
  int n= X.nrow();  //assuming the number of rows is equal to the size of Y
  int p1= X.ncol();
  NumericMatrix Z(n,p1);
  NumericVector luck(0);


  for (int i=0; i<n; ++i) {
    for (int l=0; l < p1; ++l) {
      Z(i,l)=Y[i]*X(i,l);
      }
  }

  //List result; // to store results

  for (int j = 0; j < k; ++j) {
    IntegerVector sampledIndices=Rcpp::sample(n, p, false);
    std::sort(sampledIndices.begin(), sampledIndices.end());
    //initialize the result matrices for sampled rows
    NumericMatrix sampledX(p, X.ncol());
    NumericMatrix sampledZ(p, Z.ncol());

    //populate sampled submatrices using sampled indices
    for (int i = 0; i < p; ++i){
      sampledX.row(i) = X.row(sampledIndices[i] - 1); //for 0-based indexing
      sampledZ.row(i) = Z.row(sampledIndices[i] - 1);
    }

    NumericVector a=as<NumericVector>(binaryToInt(sampledX));
    NumericVector b=as<NumericVector>(binaryToInt(sampledZ));

    List ps=pairsearch11(sorted_index_vector(a),sorted_index_vector(b), stl_sort(a),stl_sort(b));
    int s=ps.size();

    if(s>0){
    for(int i=0;i<s;++i){
      IntegerMatrix pr=(ps[i]);
      NumericVector pv=cantor_map(as<NumericMatrix>(pr));
      for(int j=0;j<pv.size();++j){
        luck.push_back(pv[i]);
        //luck.attr("dim")=Dimension(2,int(luck.length()/2));
      }

    }
    }
    //result.push_back(pairsearch11 (sorted_index_vector(a),sorted_index_vector(b), stl_sort(a),stl_sort(b) ));
  }
  //luck.attr("dim")=Dimension(2,int(luck.length()/2));
    NumericMatrix ans;
    if(luck.size()>0){
      ans=inverse_cantor_map(luck);
    }
    else{
      ans=ans;
    }
    return ans;
  //return result;

    // append sampled submatrices to the result list
    //result.push_back(pair_search4(as<NumericVector>(binaryToInt(sampledZ)),as<NumericVector>(binaryToInt(sampledX))));
  //}

  //return result;
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
  //List result; // to store results

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
  NumericMatrix ans;
  if(luck.size()>0){
    ans=inverse_cantor_map(luck);
  }
  else{
    ans=ans;
  }
  return ans;
  //luck.attr("dim")=Dimension(2,int(luck.length()/2));
  //return inverse_cantor_map(luck);
  //return result;
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

  return wrap(indices);  //convert std::vector to Rcpp IntegerVector
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
List lassorisky(NumericVector Y, NumericMatrix X, double lambda, IntegerVector nzero_indices_beta, IntegerVector nzero_indices_theta, int max_iter = 1000, double tol = 1e-6) {
  int n = X.nrow();
  int p = X.ncol();
  int num_interactions = p*(p + 1) / 2;
  NumericVector beta(p);
  NumericVector theta(num_interactions);
  NumericVector beta_old(p);
  NumericVector theta_old(num_interactions);
  int nzerobet=nzero_indices_beta.size();
  //int nzerothet=nzero_indices_theta.size();

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

    //updating]] beta
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

    //updating theta
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
  //int nzerothet = nzero_indices_theta.size();

  for (int iter = 0; iter < max_iter; ++iter) {
    std::copy(beta.begin(), beta.end(), beta_old.begin());
    std::copy(theta.begin(), theta.end(), theta_old.begin());

    NumericVector residual(Y);

    //subtracting contribution of beta coefficients
    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < p; ++j) {
        if (beta[j] != 0) {
          residual[i] -= X(i, j) * beta[j];
        }
      }
    }

    //subtracting the contribution of theta coefficients
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

    //updating beta
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

    //updating theta
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

    //checking for convergence
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
List lassorisky3(NumericVector Y, NumericMatrix X, double lambda, IntegerVector zero_indices_beta, IntegerVector zero_indices_theta, int max_iter = 1000, double tol = 1e-6) {
  int n = X.nrow();
  int p = X.ncol();
  int num_interactions = p * (p + 1) / 2;
  NumericVector beta(p);
  NumericVector theta(num_interactions);
  NumericVector beta_old(p);
  NumericVector theta_old(num_interactions);
  //int zerobet=zero_indices_beta.size();
  //int zerothet=zero_indices_theta.size();

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

    //updating beta
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
          //interaction terms
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

// [[Rcpp::export]]
NumericMatrix createInteractionMatrix(NumericMatrix X, IntegerVector indices) {
  int n = X.nrow();
  int p = X.ncol();
  int k = indices.size();
  NumericMatrix interactionMatrix(n, k);

  for (int idx = 0; idx < k; ++idx) {
    int interactionIndex = indices[idx];

    //finding the column indices (i, j) for the interactionIndex
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

    //filling the interaction matrix with the product of the respective columns
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
  //double epsilon=1e-10;

  for (int iter = 0; iter < max_iter; ++iter) {
    std::copy(beta.begin(), beta.end(), beta_old.begin());
    std::copy(theta.begin(), theta.end(), theta_old.begin());

    NumericVector residual = clone(Y);

    //subtracting the contribution of beta coefficients
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

      //getting the column indices (i, j) for the interactionIndex
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

    //updating beta
    for (int b = 0; b < nzerobet; ++b) {
      int j = nzero_indices_beta[b] - 1;
      double X_jY = 0;
      double X_jX_j = 0;

      for (int i = 0; i < n; ++i) {
        double r_ij=residual[i] + X(i, j)*beta[j];
        if(r_ij!=0){
        //{
        X_jY += X(i, j) * r_ij;
        X_jX_j += X(i, j) * X(i, j);
        }
      }
      if(X_jX_j!=0){
      beta[j] = softThreshold(X_jY /X_jX_j, lambda /X_jX_j);
      }
      }

    //updating theta
    for (int t = 0; t < nzerothet; ++t) {
      int index = nzero_indices_theta[t]-1;

      //finding the column indices (i, j) for the interactionIndex
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
      theta[index] = softThreshold(W_klY / (W_klW_kl), lambda / (W_klW_kl));
      }
      }

    //checking for convergence
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


//' @export
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
        X_normalized(i, j) = 0; // If sd is 0, all elements are the same nd hence normalized to 0
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
      Y_normalized[i] = 0; // If sd is 0, all elements are same so normalized to 0
    }
  }

  return Y_normalized;
}

// [[Rcpp::export]]
int size(NumericVector lm){
  return lm.size();
}




// [[Rcpp::export]]
List computesolutionxyz(NumericMatrix X, NumericVector Y, NumericVector lambda_grid, bool binary=false) {
  int L = lambda_grid.size();
  int n = X.nrow();
  int p = X.ncol();

  List lasso_solutions(L);

  IntegerVector A(0);
  IntegerVector B(0);

  for (int l = 0; l < L; ++l) {
    double lambda = lambda_grid[l];

    //compute LASSO solution
    List solution = computesolution(Y, X, A, B, lambda);
    NumericVector beta = solution["beta"];
    NumericVector theta = solution["theta"];

    //check KKT conditions/not considering additional vecs
    //IntegerVector U;
    //IntegerVector V;

    //compute residuals to push necessary indices
    NumericVector residual = clone(Y);
    for (int j = 0; j < A.size(); ++j) {
      //int col = A[j] - 1;  //convert 1-based index to 0-based
      //double beta_j = beta[j];
      for (int i = 0; i < n; ++i) {
        residual[i] -= X(i, A[j] - 1) * beta[j];
      }
    }

    int interaction_index = 0;
    int B_index = 0;

    for (int k = 0; k < p; ++k) {
      for (int l = k; l < p; ++l) {
        //checking if the current interaction index is in B
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

    //updating U and V based on KKT conditions
    for (int k = 0; k < p; ++k) {
      double XT_k_residual = sum(X(_, k) * residual) / n;
      if (std::abs(XT_k_residual) > lambda) {
        A.push_back(k + 1);
      }
    }

    IntegerVector eqpairs1;
    //incorporating lasso
    //List eqpairs=transform_pairs(pair_search4(residual, X));
    if(binary){
    eqpairs1=getInteractionColumn(getunique(uniformSampling(X,residual,3,3)), p);}
    else{
    eqpairs1=getInteractionColumn(getunique(weightedSampling(X,residual,3,3)), p);}


       //getunique(weightedSampling(X,residual,3,3));

    //interaction terms
    //int num_interactions = p * (p + 1) / 2;
    for(int i=0; i< eqpairs1.size();++i){
      B.push_back(i);
    }
    //for (int k = 0; k < p; ++k) {

      //for (int l = k; l < p; ++l) {

        //this is not computationally efficient
        //what
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
    //if (U.size() == 0 && V.size() == 0) {
    //  lasso_solutions[l] = List::create(Named("beta") = beta, Named("theta") = theta);
    //  break;
    //} else {
      // Update A and B
      //for (int u = 0; u < U.size(); ++u) {
      //  if (std::find(A.begin(), A.end(), U[u]) == A.end()) {
      //    A.push_back(U[u]);
      //  }
      //}
      //for (int v = 0; v < V.size(); ++v) {
      //  if (std::find(B.begin(), B.end(), V[v]) == B.end()) {
      //    B.push_back(V[v]);
      //  }
      //}
    //}

    lasso_solutions[l] = List::create(Named("beta") = beta, Named("theta") = theta);
  }

  return lasso_solutions;
}




// [[Rcpp::export]]
NumericMatrix generate_interaction_matrix(NumericMatrix X) {
  int n = X.nrow();
  int p = X.ncol();
  int num_interactions = p * (p + 1) / 2;

  //init the interaction matrix W
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
  NumericVector D = Rcpp::runif(M); //generating a vector D with independent components from U[0, 1]

  NumericVector R(n); //initializing vector R with zeros
  NumericVector indices1(M);
  IntegerVector v = seq(1,n);
  // sampling
  indices1 = Rcpp::sample(v, M, with_replacement);

  for (int i = 0; i < M; ++i) {
    indices1[i] = indices1[i] - 1;
  }

  for (int i = 0; i < n; ++i) {
    IntegerVector ind = findIndex(indices1, i);
    float sum1 = 0.0; //declaring and initializing sum1
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

  //calculate Z(n x p) matrix
  NumericMatrix Z(n, p);
  for (int i=0; i<n; ++i) {
    for (int l=0; l < p;++l) {
      Z(i,l)=Y[i]*X(i,l);
    }
  }

  //calculate the interaction strength gamma_jk
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


/***R
#set.seed(123)  # For reproducibility

#parameters
n<- 10  #no: of observations
p<- 5   #no: of features

#CASE 1- RANDOM SAMPLING (cont. case test for computesolution)
#------------------------------------------------------------

#set.seed(123)
n <-100
p <-5
X <- matrix(sample(1:100, n * p, replace = TRUE), n, p)
X<-normalizeMatrix(X)
beta_true <- c(0, 1, 0, 2, 0) #the beta coefficient values
theta_true <- c(0, 1,6,8, 99,88,61,43,8,0,0,80,48,0,0) #theta coefficient values
Y <- X %*% beta_true + generate_interaction_matrix(X)%*%theta_true + rnorm(n)
lambda <-0.1
nzero_indices_beta <- c(2,4)  #non-zero indices in beta
nzero_indices_theta <- c(2,3,4,5,6, 7,8, 9, 12, 13) #non-zero indices in theta
computesolution(Y, X, nzero_indices_beta, nzero_indices_theta,0.5)

#RESULTS
#-------

#> computesolution(Y, X, zero_indices_beta, zero_indices_theta,0.5)
#$beta
#[1] 0.0000000 0.9922276 0.0000000 1.9792329 0.0000000

#$theta
#[1]  0.000000  1.135849  5.791832  8.070993 98.980953 88.120388 61.092121 42.954470  8.012439  0.000000
#[11]  0.000000 79.891271 47.821030  0.000000  0.000000

#CASE 2- INTERACTION SEARCH FOR BINARY Y w/ toeplitz cov
#-----------------------------------------------

#creating a Toeplitz covariance matrix

rho <- 0.5  #correlation parameter
sigma <- toeplitz(rho^(0:(p-1)))

library(MASS)
X_1 <- mvrnorm(n, mu = rep(0, p), Sigma = sigma) #gaussian design matrix
X_design <- ifelse(X_1 > 0, 1, 0)  #transform the values: -ve to 0, +ve to 1
Yd<-runif(100, min = -1,max = 1)
Ydbin<-ifelse(Yd > 0, 1, -1)
res=uniformSampling(X_design,Ydbin, 10, 50) #uniform sampling and finding eq pairs using pairsearch11()
getunique(res) #to get unique pairs of integers

#RESULTS
#-------

#>res=uniformSampling(X_design,Ydbin, 10, 45); res
#[,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16] [,17] [,18] [,19]
#[1,]    3    1    3    3    3    2    2    2    2     1     2     2     0     2     2     3     0     0     0
#[2,]    3    1    2    2    3    2    2    2    2     1     1     1     0     2     2     3     0     0     0
#[,20] [,21] [,22] [,23]
#[1,]     4     1     0     1
#[2,]     4     1     0     1

#>getunique(res)
#[,1] [,2] [,3] [,4] [,5] [,6] [,7]
#[1,]    0    1    2    2    3    3    4
#[2,]    0    1    1    2    2    3    4


#CASE 3-INTERACTION SEARCH FOR CONTINUOUS Y w/ toeplitz cov
#----------------------------------------------------------

#create a Toeplitz covariance matrix

rho2 <- 0.3  #correlation parameter
sigma2 <- toeplitz(rho^(0:(p-1)))
X_2 <- mvrnorm(n, mu = rep(0, p), Sigma = sigma2) #gaussian design matrix
X_design2 <- ifelse(X_2 > 0, 1, -1)  #transform the values: -ve to -1, +ve to 1
Yd2<-runif(100, min = -1,max = 1)
Ydn2<-normalizeVector(Yd2) #dont have to normalise as normalisation is already done w/in sampling functions
res2=weightedSampling(X_design2,Yd2, 20, 50) #weighted sampling and finding eq pairs using pairsearch11()
getunique(res2) #to get unique pairs of integers

#RESULTS
#-------

#> res2=weightedSampling(X_design2,Yd2, 10, 30);res2; #weighted sampling and finding eq pairs using pairsearch11()
#[,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16] [,17] [,18] [,19]
#[1,]    3    0    0    0    0    2    0    0    0     0     0     0     0     0     0     0     0     0     0
#[2,]    3    0    0    0    0    2    0    0    0     0     0     0     0     0     0     0     0     0     0
#[,20] [,21] [,22] [,23] [,24] [,25] [,26] [,27] [,28] [,29] [,30] [,31] [,32] [,33] [,34] [,35] [,36] [,37]
#[1,]     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     2     0
#[2,]     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     2     0
#[,38] [,39] [,40] [,41] [,42] [,43] [,44] [,45] [,46] [,47] [,48] [,49] [,50] [,51] [,52] [,53] [,54] [,55]
#[1,]     0     0     0     2     0     0     0     0     3     0     0     0     0     1     0     0     0     0
#[2,]     0     0     0     2     0     0     0     0     3     0     0     0     0     1     0     0     0     0
#[,56] [,57] [,58] [,59] [,60] [,61] [,62] [,63] [,64] [,65] [,66] [,67] [,68] [,69] [,70] [,71] [,72] [,73]
#[1,]     2     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
#[2,]     2     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0
#[,74] [,75] [,76] [,77] [,78] [,79] [,80] [,81] [,82] [,83] [,84] [,85] [,86] [,87] [,88] [,89] [,90] [,91]
#[1,]     0     0     2     0     0     0     0     3     0     0     0     0     3     0     0     0     0     0
#[2,]     0     0     2     0     0     0     0     3     0     0     0     0     3     0     0     0     0     0
#[,92] [,93] [,94] [,95] [,96] [,97] [,98] [,99] [,100] [,101] [,102] [,103] [,104] [,105] [,106] [,107]
#[1,]     0     0     0     0     4     0     0     0      0      2      0      0      0      0      1      1
#[2,]     0     0     0     0     4     0     0     0      0      2      0      0      0      0      1      1
#[,108] [,109] [,110] [,111] [,112] [,113] [,114] [,115] [,116] [,117] [,118] [,119] [,120] [,121] [,122]
#[1,]      1      1      0      0      0      2      0      0      0      0      2      0      0      0      0
#[2,]      1      1      0      0      0      2      0      0      0      0      2      0      0      0      0
#[,123] [,124] [,125] [,126] [,127] [,128] [,129] [,130] [,131] [,132] [,133] [,134] [,135] [,136] [,137]
#[1,]      1      1      1      1      0      0      0      1      1      1      1      0      0      0      4
#[2,]      1      1      1      1      0      0      0      1      1      1      1      0      0      0      4
#[,138] [,139] [,140] [,141] [,142] [,143] [,144] [,145] [,146] [,147] [,148] [,149] [,150] [,151] [,152]
#[1,]      0      0      0      0      0      0      0      0      0      3      0      0      0      0      2
#[2,]      0      0      0      0      0      0      0      0      0      3      0      0      0      0      2
#[,153] [,154] [,155] [,156]
#[1,]      0      0      0      0
#[2,]      0      0      0      0
#> getunique(res2) #to get unique pairs of integers
#[,1] [,2] [,3] [,4] [,5]
#[1,]    0    1    2    3    4
#[2,]    0    1    2    3    4


#CASE 4- INTERACTION SEARCH FOR BINARY Y w/ randomized matrix
#-----------------------------------------------

#create a random matrix and vector w/values from -1 to 1
X4 <- matrix(runif(n * p, min = -1, max = 1), nrow =n, ncol =p)
X4_d <- ifelse(X4 > 0, 1, -1)
Y4<-runif(100, min = -1,max = 1)
Y4_d <- ifelse(Y4 > 0, 1, -1)
uniformSampling(X4_d,Y4_d,10,50)

#RESULTS
#-------

#> res4=uniformSampling(X4_d,Y4_d,10,50)
#> res4
#[,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13] [,14] [,15] [,16] [,17]
#[1,]    2    2    0    2    2    1    2    4    0     3     0     1     0     0     0     2     2
#[2,]    2    2    0    2    2    1    2    4    0     3     0     1     0     0     0     2     2
#> getunique(res4)
#[,1] [,2] [,3] [,4] [,5]
#[1,]    0    1    2    3    4
#[2,]    0    1    2    3    4

#CASE 5- INTERACTION SEARCH FOR CONTINUOUS Y w/ randomized matrix
#----------------------------------------------------------------

#create a random matrix and vector w/values from -1 to 1
X5 <- matrix(runif(n * p, min = -1, max = 1), nrow =n, ncol =p)
X5_d <- ifelse(X5 > 0, 1, -1)
Y5<-runif(100, min = -1,max = 1)
weightedSampling(X5_d,Y5,10,40)
getunique(weightedSampling(X5_d,Y5,10,40))
getunique(weightedSampling(X5_d,Y5,30,40))
getunique(weightedSampling(X5_d,Y5,45,40))
getunique(weightedSampling(X5_d,Y5,45,60))


#RESULTS
#-------

#> getunique(weightedSampling(X5_d,Y5,10,40))
#[,1] [,2] [,3] [,4] [,5]
#[1,]    0    1    2    3    4
#[2,]    0    1    2    3    4

#> getunique(weightedSampling(X5_d,Y5,30,40))
#[,1] [,2] [,3] [,4] [,5]
#[1,]    0    1    2    3    4
#[2,]    0    1    2    3    4

#> getunique(weightedSampling(X5_d,Y5,45,40))
#[,1] [,2] [,3] [,4] [,5]
#[1,]    0    1    2    3    4
#[2,]    0    1    2    3    4

#> getunique(weightedSampling(X5_d,Y5,45,60))
#[,1] [,2] [,3] [,4] [,5]
#[1,]    0    1    2    3    4
#[2,]    0    1    2    3    4

#CASE 6- 500 X 5 RANDOM SAMPLING (cont. case test for computesolution)
#------------------------------------------------------------

n1 <-500
p1 <-5
Xq <- matrix(sample(1:500, n1 * p1, replace = TRUE), n1, p1)
Xq<-normalizeMatrix(Xq)
beta_true1 <- c(0, 1, 4.5, 9, 50) #the beta coefficient values
theta_true1 <- c(0, 1,6,0, 0,23,1,4,8,0,0,9.5,4.8,10,0) #theta coefficinet values
Yq <- Xq %*% beta_true1 + generate_interaction_matrix(Xq)%*%theta_true1 + rnorm(n1)
#lambda <-0.1
nzero_indices_beta1 <- c(2,3,4,5)  #non-zero indices in beta
nzero_indices_theta1 <- c(2,3,6, 7,8, 9, 12, 13,14) #non-zero indices in theta
computesolution(Yq, Xq, nzero_indices_beta1, nzero_indices_theta1,0.5)

#RESULTS
#-------

#> computesolution(Yq, Xq, nzero_indices_beta1, nzero_indices_theta1,0.5)
#$beta
#[1]  0.0000000  0.9069347  4.5303639  9.0262536 49.9705656

#$theta
#[1]  0.0000000  0.9705311  6.0368197  0.0000000  0.0000000 22.9936102  0.9457652  3.9673389  8.0219062  0.0000000
#[11]  0.0000000  9.4622569  4.7832820 10.0098533  0.0000000

#CASE 7- 1000 X 5 RANDOM SAMPLING (cont. case test for computesolution)
#------------------------------------------------------------

n2 <-1000
p2 <-5
Xp <- matrix(sample(1:950, n2 * p2, replace = TRUE), n2, p2)
Xp<-normalizeMatrix(Xp)
beta_true2 <- c(0, 1, 4.5, 9, 50) #the beta coefficient values
theta_true2 <- c(0, 1,6,0, 0,23,1,4,8,0,0,9.5,4.8,10,0) #theta coefficinet values
Yp <- Xp %*% beta_true2 + generate_interaction_matrix(Xp)%*%theta_true2 + rnorm(n2)
nzero_indices_beta2 <- c(2,3,4,5)  #non-zero indices in beta
nzero_indices_theta2 <- c(2,3,6, 7,8, 9, 12, 13,14) #non-zero indices in theta
computesolution(Yp, Xp, nzero_indices_beta2, nzero_indices_theta2,0.5)

#RESULTS
#-------

#> computesolution(Yp, Xp, nzero_indices_beta2, nzero_indices_theta2,0.5)
#$beta
#[1]  0.000000  1.000353  4.482866  9.019077 50.017071

#$theta
#[1]  0.000000  1.067835  5.991539  0.000000  0.000000 23.007216  1.072638  4.049473  7.960569  0.000000  0.000000
#[12]  9.489147  4.830940 10.023127  0.000000


#CASE 8- 1500 X 5 RANDOM SAMPLING (cont. case test for computesolution)
#------------------------------------------------------------

n3 <-1500
p3 <-5
Xw <- matrix(sample(1:1450, n3* p3, replace = TRUE), n3, p3)
Xw<-normalizeMatrix(Xw)
beta_true3 <- c(0, 1.6, 4, 99, 5) #the beta coefficient values
theta_true3 <- c(0, 0,6,24, 8,2,0,4,87,10,0,0,0,60,0) #theta coefficinet values
Yw <- Xw %*% beta_true3 + generate_interaction_matrix(Xw)%*%theta_true3 + rnorm(n3)
nzero_indices_beta3 <- c(2,3,4,5)  #non-zero indices in beta
nzero_indices_theta3 <- c(3,4,5,6,8, 9,10,14) #non-zero indices in theta
computesolution(Yw, Xw, nzero_indices_beta3, nzero_indices_theta3,0.5)

#RESULTS
#-------

#> computesolution(Yw, Xw, nzero_indices_beta3, nzero_indices_theta3,0.5)
#$beta
#[1]  0.000000  1.600520  3.985663 99.042312  4.989787

#$theta
#[1]  0.000000  0.000000  5.991907 24.072027  7.988886  2.015470  0.000000  3.989898 86.961989  9.991653  0.000000
#[12]  0.000000  0.000000 59.986628  0.000000





*/



