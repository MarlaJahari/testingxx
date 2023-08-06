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


//[[Rcpp::export]]
IntegerMatrix equal_pairs(NumericVector u, NumericVector v, int max_number_of_pairs) {
  //set sizes of array
  int nu = u.size();
  int nv = v.size();
  IntegerVector ou=seq(0,nu-1);
  IntegerVector ov=seq(0,nv-1);

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
    // too many pairs kill the search
    if(count > max_number_of_pairs) {
      break;
    }
  }
  int n = 0;
  //fill pairs in a 2x(pairs) matrix
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


// [[Rcpp::export]]
NumericMatrix push(IntegerMatrix M, IntegerVector new_vector) {
  if (M.nrow() != new_vector.size()) {
    Rcpp::stop("Dimensions of the matrix and vector do not agree.");
  }

  int nrow = M.nrow();
  int ncol = M.ncol();

  NumericMatrix new_matrix(nrow + 1, ncol);

  // copy elements from the old matrix to the new matrix
  for (int i = 0; i < nrow; ++i) {
    for (int j = 0; j < ncol; ++j) {
      new_matrix(i, j) = M(i, j);
    }
  }

  // append the new vector as the last row of the new matrix
  for (int j = 0; j < ncol; ++j) {
    new_matrix(nrow, j) = new_vector[j];
  }

  return new_matrix;
}


// Function to convert -1 to 0 and convert binary columns to integer values
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


// Function to find pair matches between vectors x and z
// [[Rcpp::export]]
NumericMatrix find_pair_matches(IntegerVector x0,IntegerVector z0) {
  int p = x0.size();
  int n = z0.size();
  int total_pairs = p * (p);
  NumericMatrix result(2, total_pairs);

  NumericVector x = as<NumericVector>(x0);
  NumericVector z = as<NumericVector>(z0);

  // Sort the input vectors
  std::vector<double> new_vec_1(x.begin(), x.end());
  std::vector<double> new_vec_2(z.begin(), z.end());
  std::sort(new_vec_1.begin(), new_vec_1.end());
  std::vector<double> temp=new_vec_1;

  // Find pair matches and store the indices
  int col = 0;
  for (int i = 0; i < n; ++i){
    new_vec_1=temp;
    if(std::binary_search(new_vec_1.begin(), new_vec_1.end(), new_vec_2[i])){
      result(0, col) = std::distance(new_vec_1.begin(), std::lower_bound(new_vec_1.begin(), new_vec_1.end(), new_vec_2[i])) + 1;
      result(1, col) = i + 1;
      //new_vec_1[std::distance(new_vec_1.begin(), std::lower_bound(new_vec_1.begin(), new_vec_1.end(), new_vec_2[i]))]+=0.00001; //s0 that the iterator will skip this, but still count index
      col++;
      //new_vec_1=cut_first_element(new_vec_1);
    }

  }

  // Resize the result matrix to remove unused columns
  result = result(_, Range(0, col - 1));

  return result;
}



// function implementing final xyz
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
    IntegerMatrix eq=equal_pairs(x,z,50);
    for(int i=0;i < eq.ncol();++i){

      if(interaction_strength(X,Y,eq(_,i)[0],eq(_,i)[1])>gamma){
      push(I,eq(_,i));
      }

    }
  }

  return I;


}


// function implementing final xyz
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
  IntegerVector x=binaryToInt(X);
  IntegerVector z=binaryToInt(Z);

  return find_pair_matches(x,z);


}

// Function that cuts everything it sees
// [[Rcpp::export]]
std::vector<double> cut(const std::vector<double>& input_vec) {
  return std::vector<double>(input_vec.begin(), input_vec.end()-1);
}

// [[Rcpp::export]]
bool bin(NumericVector vec){
 return std::binary_search(vec.begin(), vec.end(), 2);
}


// [[Rcpp::export]]
NumericMatrix generateMatrix(int n, int p) {
  NumericMatrix matrix(n, p);
  return matrix;
}

// Function to create a random binary matrix
// [[Rcpp::export]]
NumericMatrix random_binary_matrix(int n, int p) {
  NumericMatrix mat(n, p);

  // Set random seed (optional)
  // srand(time(NULL));

  // Fill the matrix with random binary values
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < p; ++j) {
      // Generate a random value between 0 and 1
      double random_value = R::runif(0, 1);

      // Convert to binary (0 or 1)
      mat(i, j) = (random_value < 0.5) ? 0 : 1;
    }
  }

  return mat;
}

// Function to generate a random binary vector of size n with elements -1 and 1
// [[Rcpp::export]]
NumericVector random_binary_vector(int n) {
  NumericVector binaryVector(n);

  // Set seed for reproducibility
  srand(time(0));

  for (int i = 0; i < n; i++) {
    int randomNumber = rand() % 2; // Generate random number 0 or 1
    binaryVector[i] = (randomNumber == 0) ? 0 : 1; // Convert 0 to -1, 1 to 1
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
IntegerVector goo(int n) {
  IntegerVector vec={5,2,2,3};//seq(1,n); // Create a NumericVector with size n
  return vec;
}



