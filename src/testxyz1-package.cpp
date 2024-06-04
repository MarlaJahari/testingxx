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
    double x= t-w;
    double y= w;

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




//  perform weighted sampling from matrices X and Z
// [[Rcpp::export]]
List weightedSampling(NumericMatrix X, NumericVector Y, int p, int k) {
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
    result.push_back(pair_search4(as<NumericVector>(binaryToInt(sampledX)),as<NumericVector>(sampledZ)));
  }

  return result;
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


//soft-thresholding function
NumericVector soft_threshold(NumericVector x, double lambda) {
  NumericVector result=abs(x)-lambda;
  return ifelse(result > 0.0, result, 0.0);
}

//function to compute Lasso solution for a given lambda
List compute_lasso_solution(const NumericMatrix& X, const NumericVector& Y, const IntegerVector& A, const IntegerVector& B, double lambda) {
  //int n= X.nrow();
  int p= X.ncol();

  //subset design matrix and response vector based on active sets A and B
  NumericMatrix X_A=X; //(_, A);
  NumericVector Y_B=Y; //- X(_, B) * 0.0;  //for indices of THETA, so θ_B is set to 0

  //compute lasso solution
  NumericVector beta_hat_A = 0.0; //solve(t(X_A) * X_A + lambda * n * diag(p), t(X_A) * Y_B);
  NumericVector theta_hat_B = 0.0;  //placeholder for theta_B since B is known

  //update beta and theta for the full vector
  NumericVector beta_hat(p,0.0);
  NumericVector theta_hat(p,0.0);

  beta_hat[A]= beta_hat_A;
  theta_hat[B]= theta_hat_B;

  return List::create(_["beta"] = beta_hat, _["theta"] = theta_hat);
}

//active set strategy for lasso computation
List active_set_lasso(const NumericMatrix& X, const NumericVector& Y, const NumericVector& lambda_values) {
  int p=X.ncol();
  int L=lambda_values.size();

  //initialize solution vectors
  NumericMatrix beta_solutions(p, L);
  NumericMatrix theta_solutions(p, L);

  IntegerVector A, B;

  for (int l=0; l<L; ++l) {
    if (l>0) {
      A=seq_len(p); //[beta_solutions(_, l - 1) != 0.0];
      B=seq_len(p); //[theta_solutions(_, l - 1) != 0.0];
    }

    //compute Lasso solution for the current lambda
    List solution =compute_lasso_solution(X, Y, A, B, lambda_values[l]);
    NumericVector beta_hat= solution["beta"];
    NumericVector theta_hat= solution["theta"];

    // find sets U and V based on KKT conditions
    NumericVector kkt_cond = abs(transpose(X)*(Y - X*beta_hat - theta_hat))/X.nrow();
    IntegerVector U= seq_len(p); //[kkt_cond > lambda_values[l]];
    IntegerVector V= intersect(A, U);

    if (U.size()==0 && V.size()==0) {
      //No violations so update solutions
      beta_solutions(_, l)=beta_hat;
      theta_solutions(_, l)=theta_hat;
    } else{
      //update A and B and repeat
      A= union_(A, U);
      B= union_(B, V);
      l--; // repeating with the same lambda
    }
  }

  return List::create(_["beta"] = beta_solutions, _["theta"] = theta_solutions);
}

