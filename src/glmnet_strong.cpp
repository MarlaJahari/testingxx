#include <RcppArmadillo.h>
using namespace Rcpp;

//arma::mat g_start;
double GA_TOL=0.0001;

//X^T*X*w
// [[Rcpp::export]]
void calculateXXw(arma::mat X, arma::vec w, arma::vec &ret) {
  ret= trans(X)*X*w;//trans(trans(X * w)*X);
}

//max in the grid of lambda values
// [[Rcpp::export]]
double maxLambda(arma::colvec g_start) {
  //l_infinity norm of our initial matrix
  return arma::norm(g_start, "inf");
}

//done initially for checking KKT conditions
// [[Rcpp::export]]
void calculateGradient(arma::mat X, arma::vec z, double lambda, arma::colvec g_start, int n, arma::colvec&g) {
  int n_half=n/2;
  //this is initializing for the method outlined in "cleaner,faster" paper, split x into u &l
  arma::vec u= z.subvec(0, n_half- 1);
  arma::vec l= z.subvec(n_half, n- 1);
  arma::vec w=u- l;
  arma::vec XXw;

  calculateXXw(X,w, XXw);
  g= g_start+ lambda;
  g.subvec(0,n_half - 1)+= XXw;
  g.subvec(n_half,n - 1)-= XXw;
}

//elastic net value for a given problem (made it lasso)
//eta=1, the off diagonal elements of K matrix disappears
//we'll require two evaluate functions like this
// [[Rcpp::export]]
double evaluate(arma::mat X, arma::vec y, arma::vec z, double lambda) {
  arma::vec w = z.subvec(0, X.n_cols-1)-z.subvec(X.n_cols, 2*X.n_cols- 1);
  //to change 0.5-> 1/2n where n is the size of our response vector
  return 0.5*arma::sum(arma::square(y- X*w))+lambda*(arma::norm(w,1));
}

//union of two vectors
// [[Rcpp::export]]
arma::uvec vunion(const arma::uvec& first, const arma::uvec& second) {
  std::vector<unsigned> output;
  std::set_union(first.begin(), first.end(), second.begin(), second.end(), std::back_inserter(output));
  return arma::uvec(output);
}

//intersection of two vectors
// [[Rcpp::export]]
arma::uvec vintersection(const arma::uvec& first, const arma::uvec& second) {
  std::vector<unsigned> output;
  std::set_intersection(first.begin(), first.end(), second.begin(), second.end(), std::back_inserter(output));
  return arma::uvec(output);
}

//difference
// [[Rcpp::export]]
arma::uvec vdifference(const arma::uvec& first, const arma::uvec& second) {
  std::vector<unsigned> output;
  std::set_difference(first.begin(), first.end(), second.begin(), second.end(), std::back_inserter(output));
  return arma::uvec(output);
}

//to find the active set outlined in strong-preds
// [[Rcpp::export]]
void findActiveSet(arma::vec& g, arma::vec& z, arma::uvec& A) {
  arma::uvec nonpos_g= find(g<= 0);
  arma::uvec pos_z= find(z> 0);
  A =arma::unique(arma::join_cols(nonpos_g, pos_z));
}

//both the functions below are optional
//i/n_i has to be active
// [[Rcpp::export]]
void sparsify(arma::vec& w, arma::vec& u, arma::vec& l) {
  w = u - l;
  arma::uvec neg_w= find(w< 0);
  arma::uvec pos_w= find(w> 0);
  u(neg_w).zeros();
  u(pos_w)= w(pos_w);
  l(pos_w).zeros();
  l(neg_w)= -w(neg_w);
}

//speeding up convergence
// [[Rcpp::export]]
void projectAndSparsify(arma::vec& w, arma::vec& u, arma::vec& l) {
  auto clamp =[](double d) {return d >=0?d: 0; };
  u.transform(clamp);
  l.transform(clamp);
  sparsify(w,u,l);
}

//server function to CJ
inline double approx(double alpha, double p, double q) {
  return p*alpha + q;
}

//based entirely on improved_glmnet's "secret sauce" function
// [[Rcpp::export]]
double aggressiveStep(const arma::uvec& A, const arma::vec& eta, arma::vec& z, const arma::vec& delz_A, const arma::vec& Kz, const arma::vec& Ku) {
  arma::vec z_A= z.elem(A);

  // find the knots.
  arma::uvec knots= find(delz_A+z_A <= 0);

  //check for failure
  if (knots.n_elem==0) return 0;

  //calculate n sort alphas
  arma::vec alphas= -z_A(knots)/delz_A(knots);
  arma::uvec sorted_indices=arma::sort_index(alphas);

  double pi=0,omega=0,sigma=0,c=0;

  for(arma::uword i=0; i< sorted_indices.n_elem; i++) {
    arma::uword indx=sorted_indices[i];

    double alpha_i=alphas[indx];
    if (alpha_i> 1) break;

    double mu_i=delz_A[knots[indx]];
    double Ku_i=Ku[knots[indx]];
    double Kz_i=Kz[knots[indx]];
    double eta_i=eta[knots[indx]];

    pi -=2*mu_i* Ku_i;
    omega+=-mu_i*(Kz_i + eta_i) + alpha_i* mu_i*Ku_i;
    sigma+= mu_i;
    c +=alpha_i*mu_i;

    double p=pi +sigma*sigma;
    double q= omega -sigma*c;

    if (i <sorted_indices.n_elem-1 && approx(alphas[sorted_indices[i + 1]], p, q)< 0) {
      continue;
    }

    if (approx(alpha_i, p, q) >= 0) {
      return alpha_i; // will be <=1
    } else {
      return std::min(1.0, -p/q);
    }
  }

  return 1;
}

// [[Rcpp::export]]
double conservativeStep(const arma::uvec& A, arma::vec& z, const arma::vec& delz_A) {
  arma::vec z_A = z.elem(A);
  const arma::uvec neg_delz= find(delz_A < 0);
  const arma::uvec pos_z= find(z_A > 0);

  arma::uvec knots;
  knots=vintersection(neg_delz, pos_z);

  // check for failure
  if (knots.n_elem== 0) return 0;

  //calculate alphas as the step lengths necessary to clamp the values at the knots.
  arma::vec alphas= z_A(knots) / delz_A(knots);
  double alpha=std::min(-arma::max(alphas), 1.0);

  // alpha must be greater than 0.
  assert(alpha>0);

  return alpha;}

//to make a decision on whether the loop should be stopped
// [[Rcpp::export]]
bool update(arma::vec& z, const arma::uvec& A, const arma::vec& delz_A, const arma::vec& Kz, const arma::vec& Ku, const arma::vec& eta) {
  //get the largest step possible.
  double alpha= aggressiveStep(A, eta, z, delz_A, Kz, Ku);

  if(alpha== 0) {
    //failure to get a step length
    alpha=conservativeStep(A, z, delz_A);

    //update the vector.
    if(alpha==0) return false;
  }

  z.elem(A)+=delz_A*alpha;
  return true;
}

//through conjugate gradient
// [[Rcpp::export]]
void solve1(const arma::mat& A,const arma::vec& b, arma::vec& x, arma::vec&r, arma::vec&p, double &prev_r_sq_sum, bool restart, size_t iterations) {
  if (restart) {
    r=A*x-b;
    p=-r;
    prev_r_sq_sum = arma::dot(r, r);
  }

  for (size_t i = 0; i<iterations; i++) {
    arma::vec Ap= A*p;
    double alpha=prev_r_sq_sum/arma::dot(p, Ap);

    x+= alpha*p;
    r+= alpha*Ap;
    double r_sq_sum= arma::dot(r, r);

    double beta=r_sq_sum/prev_r_sq_sum;
    p*= beta;
    p-= r;
    prev_r_sq_sum= r_sq_sum;
  }
}

//this is the main solution algorithm that'll serve the sequential solution algorithm
// [[Rcpp::export]]
size_t solve(arma::mat X, arma::vec g_start, int n, arma::vec& z, arma::vec& g, double lambda, arma::uvec& whitelisted, size_t max_iterations) {
  assert(lambda>0);
  if (max_iterations==0) {
    max_iterations=z.n_elem;
  }
  int n_half=n/2;
  arma::mat XX=trans(X);
  arma::mat K_A;
  size_t i;
  arma::uvec A, A_prev;
  arma::vec delz_A, g_A;

  arma::mat XX_I= XX;
  arma::mat K=join_vert(join_horiz(XX_I, -XX), join_horiz(-XX, XX_I));
  arma::vec g_bias=g_start+lambda;

  //extracting upper and lower bounds from z.

  arma::vec u= z.subvec(0, n_half - 1);
  arma::vec l= z.subvec(n_half, n - 1);
  arma::vec w=u-l;

  //this is the main loop for optimization
  for (i=0; i<max_iterations; i++) {
    g=g_bias + K*z;

    findActiveSet(g,z,A);

    //checking for common elements between white list and active list
    if (whitelisted.size()!=0) {
      A=vintersection(A, whitelisted);
    }

    //break when A is empty
    if (A.n_elem== 0) break;
    delz_A.zeros(A.n_elem);
    g_A= -g.elem(A);
    arma::vec r=K_A*delz_A-g_A;
    arma::vec p=-r;
    double prev_r_sq_sum=arma::dot(r,r);

    //check if the active set A has changed at all.
    if (A.n_elem== A_prev.n_elem && arma::accu(A== A_prev)== A.n_elem) {
      solve1(K_A, g_A, delz_A,r,p,prev_r_sq_sum, false, 3);
    } else {
      //if so, update the submatrix and solve using primary solve func
      K_A= K.submat(A, A);
      solve1(K_A, g_A, delz_A,r,p,prev_r_sq_sum, true, 3);
      A_prev= A;
    }

    //break the loop if l_2 norm of g_A iss less than a threshold
    if (arma::norm(g_A, 2) <= 1e-4) break;

    arma::vec Kz_A= K_A*z.elem(A);
    arma::vec Ku_A= K_A*delz_A;

    if (!update(z, A, delz_A, Kz_A, Ku_A, g_bias.elem(A))) break;
    projectAndSparsify(w, u, l);
  }

  return i;
}

//final solution func thats based on glmnet +strong pred combined strategy
//returns iterations but changes n shrinks values by reference
// [[Rcpp::export]]
size_t sequential_solve(arma::mat X, int n,arma::vec& z, arma::vec g_start, arma::uvec& ever_active,
                        double lambda, double prev_lambda,
                        size_t max_iterations) {
  arma::vec u = z.subvec(0, z.n_elem/2 -1);
  arma::vec l = z.subvec(z.n_elem/2, z.n_elem-1);
  arma::vec w = u-l;

  arma::uvec whitelist=ever_active;
  size_t iters=0;

  // forming the initial strong set
  arma::vec yX=g_start.subvec(z.n_elem/2, z.n_elem-1);
  arma::vec val;
  calculateXXw(w,val,val);
  arma::uvec strong=find(arma::abs(yX-val) >= (2*lambda - prev_lambda)); //rule (34) in the paper
  strong=join_vert(strong, strong+ z.n_elem/2);

  arma::vec g;

  while(true) {
    iters+= solve(X,g_start,n,z, g,lambda,whitelist, max_iterations);

    // this is where we use xyz instead of KKT Violations
    arma::uvec active;
    calculateGradient(X,z,lambda, g_start, n,g);
    findActiveSet(g,z,active);

    arma::uvec not_in_active;
    not_in_active=vdifference(active, whitelist);

    arma::uvec in_strong;
    in_strong=vintersection(not_in_active, strong);

    // if there are violations in the strong set,
    // place them into the whitelist and repeat optimization .
    if(in_strong.n_elem> 0) {
      whitelist=vunion(in_strong, whitelist);
    } else{
      //check for the non-strong variables.
      arma::uvec in_neither_set;
      in_neither_set=vdifference(not_in_active, strong);
      //if there are any in neither of the sets, add to glmnets everactive set, then re-compute the strong set, and repeat the optimization

      if (in_neither_set.n_elem > 0) {
        ever_active=vunion(ever_active, in_neither_set);
        //use the gradient to get XXw, made changes to make this a lasso problem

        val=g.subvec(0, z.n_elem/2 - 1)-g_start.subvec(0, z.n_elem/2 -1)-lambda;
        strong=find(arma::abs(yX - val) >= (2*lambda -prev_lambda));//chack conditions for discarding predictors
        strong=join_vert(strong,strong+z.n_elem/2);
      } else {
        break;
      }
    }
  }
  return iters;
}

