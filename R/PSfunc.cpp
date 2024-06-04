#include <Rcpp.h>
using namespace Rcpp;

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
  int right= v.size() - 1;

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

  return p;//sorted_indexes;//apply_permutation(as<NumericVector>(sorted_index_vector(x)),wrap(index_vec));//sorted_indexes;
}

// [[Rcpp::export]]
List pair_search6(NumericVector x, IntegerVector sorted_indexes) {

  int N = x.size();
  int n=int(N/2);
  //initiate list
  List list(0);

  int i=0;
  while(i<(N)){
    //initialize m to get split index
    int m=0;
    //start by checking consecutive elements
    int j=i+1;
    bool check=false;
    while (x[i] == x[j]) {
      if (!check && sorted_indexes[j] >= n && sorted_indexes[j - 1] < n) {
        m=j;
        list.push_back(sorted_indexes[Range(i, m - 1)] + 1);
        check=true; // set the flag to true once the condition is met
      }
      j++;
    }

    if(((j-i)>1)&&m>0){
      list.push_back((sorted_indexes[Range(m,j-1)])-n+1);//indexes from second vector
    }
    i=j;}// jump to next (non-repeated) value

  return list;}

  // [[Rcpp::export]]
  List pair_search7(NumericVector x, IntegerVector sorted_indexes) {

    int N = x.size();
    int n=int(N/2);
    //initiate list
    List list(0);

    int i=0;
    while(i<(N)){
      //initialize m to get split index
      int m=0;
      //start by checking consecutive elements
      int j=i+1;
      while (x[i] == x[j]) {
        j++;
      }

      if((j-i)>1){

        int temp=findindexit(sorted_indexes[Range(i,j-1)],n);

        if(temp>=n && sorted_indexes[i]!=temp ){
          m=binarySearchIndex(sorted_indexes[Range(i,j-1)],temp);
          list.push_back(sorted_indexes[Range(i, m+i - 1)] + 1);
          list.push_back((sorted_indexes[Range(m+i,j-1)])-n+1);
        }
      }

      i=j;}// jump to next (non-repeated) value

    return list;}







    //[[Rcpp::export]]
    List groupt(IntegerVector list, IntegerVector sorted, int n1) {
      int n = list.size();
      int result_start = 0;
      List list0(0);
      for (int i=0; i< n-1; i++) {

        if ((list[i]!=list[i+1]) ){
          if ((sorted[result_start]<int(n1/2)) && (sorted[i]>=int(n1/2))) {

            int m=std::upper_bound(sorted.begin()+result_start, sorted.begin()+i+1, int(n1/2)-1) - sorted.begin();
            list0.push_back(sorted[Range(result_start, m-1)]);
            list0.push_back(sorted[Range(m, i)]);
          }


          result_start = i + 1;}
      }
      if((sorted[result_start]<int(n1/2) && sorted[n1-1]>=int(n1/2))){
        int m=std::upper_bound(sorted.begin()+result_start, sorted.end(), int(n1/2)-1) - sorted.begin();
        list0.push_back(sorted[Range(result_start, m-1)]);
        list0.push_back(sorted[Range(m, n1-1)]);
      }

      return list0;
    }

    // [[Rcpp::export]]
    List group2(IntegerVector list, IntegerVector sorted, int n1) {
      int n = list.size();
      int result_start = 0;
      List list0;

      for (int i = 0; i < n ; i++) {
        if (list[i] != list[i + 1]) {
          IntegerVector vec = sorted[Range(result_start, i)];
          int s=vec.size();
          if (vec[0]<int(n1/2) && vec[s-1]>=int(n1/2)) {
            int m=std::upper_bound(vec.begin(), vec.end(), int(n1/2)-1)- vec.begin();
            list0.push_back(vec[Range(0, m-1)]);
            list0.push_back(vec[Range(m, vec.size()-1)]);}
          // }

          //list0.push_back(sorted[Range(result_start, i)]);
          result_start = i + 1;
        }
      }

      //IntegerVector result = sorted[Range(result_start, n - 1)];
      //list0.push_back(sorted[Range(result_start, n - 1)]);
      return list0;
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
      List test4(NumericVector x, IntegerVector sorted_indexes) {

        int N=x.size();
        int n=N/2;

        List list;
        int i=0;
        while (i < N) {
          int j=i+1;
          while (j<N && x[i] == x[j]) {
            if (sorted_indexes[j] >= n && sorted_indexes[j - 1] < n) {
              list.push_back(sorted_indexes[Range(i, j - 1)] + 1);
              list.push_back(sorted_indexes[Range(j, j)] - n + 1);
            }
            j++;
          }
          i=j;
        }

        return list;
      }

      // [[Rcpp::export]]
      IntegerMatrix pair_search8(NumericVector x, NumericVector y) {
        int n = x.size();
        // append elements from y to the end of x
        for (int i = 0; i<n; ++i) {
          x.push_back(y[i]);
        }
        //get size of x
        int N = x.size();
        //initiate list
        IntegerMatrix m0(2,0);
        IntegerVector sorted_indexes=sorted_index_vector(x); //(index_vec.begin(), index_vec.end());
        //sort x
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
          IntegerMatrix m1(2,0);

          //if there are repetitions & they come from different vectors, split and push to different elements of list,
          //append list to main list

          if(((j-i)>1)&&m>0){
            m1=expandGrid((sorted_indexes[Rcpp::Range(i,m-1)]+1),((sorted_indexes[Rcpp::Range(m,j-1)])-n+1));//indexes from second vector
            m0=cbind(m0,m1);//list.push_back(v);
          }
          i=j;}// jump to next (non-repeated) value

        return m0;}

        // [[Rcpp::export]]
        IntegerMatrix test3(NumericVector x, IntegerVector sorted_indexes) {

          int N = x.size();
          int n=int(N/2);
          //initiate list
          IntegerMatrix m0(2,0);

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
            IntegerMatrix m1(2,0);

            //if there are repetitions & they come from different vectors, split and push to different elements of list,
            //append list to main list

            if(((j-i)>1)&&m>0){
              m1=expandGrid((sorted_indexes[Rcpp::Range(i,m-1)]+1),((sorted_indexes[Rcpp::Range(m,j-1)])-n+1));//indexes from second vector
              m0=cbind(m0,m1);//list.push_back(v);
            }
            i=j;}// jump to next (non-repeated) value

          return m0;}
