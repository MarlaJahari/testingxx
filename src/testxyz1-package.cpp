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
IntegerVector pair_search3(NumericVector x, NumericVector y) {
  int n = x.size();
  // append elements from y to the end of x
  for (int i = 0; i<n; ++i) {
    x.push_back(y[i]);
  }
  int N = x.size();
  std::vector<int> index_vec(N);
  std::iota(index_vec.begin(), index_vec.end(), 0);  // initialize with [0, 1, 2, ...]


  // sort the index_vec based on the values in the input vector v
  std::sort(index_vec.begin(), index_vec.end(),
            [&](int i, int j) { return x[i] < x[j] || (x[i] == x[j] && i < j); });


  for(int i=0; i<N; i++){
    if(index_vec[i]>n-1){
    index_vec[i]+=10*(n-1);}
  }

  // convert the std::vector<int> to IntegerVector before returning
  IntegerVector sorted_indexes(index_vec.begin(), index_vec.end());
  IntegerMatrix s(2,n*n);
  std::sort(x.begin(),x.end());
  int i=0;
  IntegerVector p(0);
  for(i=0;i<N;++i){
    int j=i+1;
    while(x[i]==x[j]){
      if(abs(sorted_indexes[i]-sorted_indexes[j])>N){
        p.push_back(sorted_indexes[i]+1);
        p.push_back(sorted_indexes[j]-10*(n-1)-n+1);
        p.attr("dim")=Dimension(2,int(p.length()/2));}
        j++;}
    }

  return p;//orted_indexes;//apply_permutation(as<NumericVector>(sorted_index_vector(x)),wrap(index_vec));//sorted_indexes;
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
  NumericVector D = Rcpp::runif(M); // Generate vector D with independent components from U[0, 1]

  NumericVector R(n); // Initialize vector R with zeros
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



// function to calculate the interaction strength γjk for pair (j, k)
// [[Rcpp::export]]
double interaction_strength(NumericMatrix X, NumericVector Y, int j, int k) {
  int n = X.nrow();
  int p = X.ncol();
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


// function to find pair matches between vectors x and z
// [[Rcpp::export]]
NumericMatrix find_pair_matches(IntegerVector x0,IntegerVector z0) {
  int p = x0.size();
  int n = z0.size();
  NumericMatrix result(2, p*p);

  NumericVector x = as<NumericVector>(x0);
  NumericVector z = as<NumericVector>(z0);

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
      //new_vec_1=cut_first_element(new_vec_1);
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
      //new_vec_1=cut_first_element(new_vec_1);
    }

  }

  if(col>0){return result(_, Range(0, col - 1));} //cut unused columns

  else{NumericMatrix nothing(2, 1); return nothing;} //no matches
}

// function implementing final xyz (theoretical)
// [[Rcpp::export]]
IntegerMatrix strongest_pairs(NumericMatrix X, NumericVector Y, int M, int L, int gamma) {
  int n = X.nrow();
  int p = X.ncol();
  NumericMatrix Z(n,p);
  if(n!=Y.size()){
    stop("Inconsistency in Dimensions, come back later");
  }
  for (int i=0; i<n; ++i) {
    for (int l=0; l < p;++l) {
      Z(i,l) = Y[i]*X(i, l);
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
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < p; ++j) {
      // making a random value between 0 and 1
      double random_value = R::runif(0, 1);

      // convert to binary (0/1)
      mat(i, j) = (random_value < 0.5) ? 0 : 1;
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

  for (int i = 0; i < n; i++) {
    int randomNumber = rand() % 2; // generating 0/1
    binaryVector[i] = (randomNumber == 0) ? 0 : 1;
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
IntegerVector vec1(int n=0) {
  IntegerVector vec={1,2,8,3,1,1};//seq(1,n); //funcs for testing
  return vec;
}

// [[Rcpp::export]]
IntegerVector vec2(int n=0) {
  IntegerVector vec={2,1,1,5,6,2};//seq(1,n);
  return vec;
}

// [[Rcpp::export]]
IntegerVector r(int n, int minValue=0, int maxValue=10) {
  IntegerVector result(n);

  for (int i = 0; i < n; ++i) {
    result[i] = R::runif(minValue, maxValue + 1); // +1 because runif generates numbers in [a, b)
  }

  return result;
}





