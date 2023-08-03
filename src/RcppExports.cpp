// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// equalpairs
IntegerMatrix equalpairs(NumericVector u, NumericVector v, IntegerVector ou, IntegerVector ov, int max_number_of_pairs);
RcppExport SEXP _testxyz1_equalpairs(SEXP uSEXP, SEXP vSEXP, SEXP ouSEXP, SEXP ovSEXP, SEXP max_number_of_pairsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type u(uSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type ou(ouSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type ov(ovSEXP);
    Rcpp::traits::input_parameter< int >::type max_number_of_pairs(max_number_of_pairsSEXP);
    rcpp_result_gen = Rcpp::wrap(equalpairs(u, v, ou, ov, max_number_of_pairs));
    return rcpp_result_gen;
END_RCPP
}
// FibCpp0
std::vector<int> FibCpp0(int n);
RcppExport SEXP _testxyz1_FibCpp0(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(FibCpp0(n));
    return rcpp_result_gen;
END_RCPP
}
// findIndex
IntegerVector findIndex(NumericVector array, double value);
RcppExport SEXP _testxyz1_findIndex(SEXP arraySEXP, SEXP valueSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type array(arraySEXP);
    Rcpp::traits::input_parameter< double >::type value(valueSEXP);
    rcpp_result_gen = Rcpp::wrap(findIndex(array, value));
    return rcpp_result_gen;
END_RCPP
}
// generate_uniform_values
NumericVector generate_uniform_values(int n, double a, double b);
RcppExport SEXP _testxyz1_generate_uniform_values(SEXP nSEXP, SEXP aSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_uniform_values(n, a, b));
    return rcpp_result_gen;
END_RCPP
}
// generate_random_projection
NumericVector generate_random_projection(int n, int M, bool with_replacement);
RcppExport SEXP _testxyz1_generate_random_projection(SEXP nSEXP, SEXP MSEXP, SEXP with_replacementSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type M(MSEXP);
    Rcpp::traits::input_parameter< bool >::type with_replacement(with_replacementSEXP);
    rcpp_result_gen = Rcpp::wrap(generate_random_projection(n, M, with_replacement));
    return rcpp_result_gen;
END_RCPP
}
// equal_pairs
IntegerMatrix equal_pairs(NumericVector u, NumericVector v, int max_number_of_pairs);
RcppExport SEXP _testxyz1_equal_pairs(SEXP uSEXP, SEXP vSEXP, SEXP max_number_of_pairsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type u(uSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type v(vSEXP);
    Rcpp::traits::input_parameter< int >::type max_number_of_pairs(max_number_of_pairsSEXP);
    rcpp_result_gen = Rcpp::wrap(equal_pairs(u, v, max_number_of_pairs));
    return rcpp_result_gen;
END_RCPP
}
// interaction_strength
double interaction_strength(NumericMatrix X, NumericVector Y, int j, int k);
RcppExport SEXP _testxyz1_interaction_strength(SEXP XSEXP, SEXP YSEXP, SEXP jSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Y(YSEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(interaction_strength(X, Y, j, k));
    return rcpp_result_gen;
END_RCPP
}
// push
NumericMatrix push(IntegerMatrix M, IntegerVector new_vector);
RcppExport SEXP _testxyz1_push(SEXP MSEXP, SEXP new_vectorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type M(MSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type new_vector(new_vectorSEXP);
    rcpp_result_gen = Rcpp::wrap(push(M, new_vector));
    return rcpp_result_gen;
END_RCPP
}
// strongest_pairs
IntegerMatrix strongest_pairs(NumericMatrix X, NumericVector Y, int M, int L, int gamma);
RcppExport SEXP _testxyz1_strongest_pairs(SEXP XSEXP, SEXP YSEXP, SEXP MSEXP, SEXP LSEXP, SEXP gammaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type X(XSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Y(YSEXP);
    Rcpp::traits::input_parameter< int >::type M(MSEXP);
    Rcpp::traits::input_parameter< int >::type L(LSEXP);
    Rcpp::traits::input_parameter< int >::type gamma(gammaSEXP);
    rcpp_result_gen = Rcpp::wrap(strongest_pairs(X, Y, M, L, gamma));
    return rcpp_result_gen;
END_RCPP
}
// generateMatrix
NumericMatrix generateMatrix(int n, int p);
RcppExport SEXP _testxyz1_generateMatrix(SEXP nSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(generateMatrix(n, p));
    return rcpp_result_gen;
END_RCPP
}
// go
NumericVector go(int n);
RcppExport SEXP _testxyz1_go(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(go(n));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_testxyz1_equalpairs", (DL_FUNC) &_testxyz1_equalpairs, 5},
    {"_testxyz1_FibCpp0", (DL_FUNC) &_testxyz1_FibCpp0, 1},
    {"_testxyz1_findIndex", (DL_FUNC) &_testxyz1_findIndex, 2},
    {"_testxyz1_generate_uniform_values", (DL_FUNC) &_testxyz1_generate_uniform_values, 3},
    {"_testxyz1_generate_random_projection", (DL_FUNC) &_testxyz1_generate_random_projection, 3},
    {"_testxyz1_equal_pairs", (DL_FUNC) &_testxyz1_equal_pairs, 3},
    {"_testxyz1_interaction_strength", (DL_FUNC) &_testxyz1_interaction_strength, 4},
    {"_testxyz1_push", (DL_FUNC) &_testxyz1_push, 2},
    {"_testxyz1_strongest_pairs", (DL_FUNC) &_testxyz1_strongest_pairs, 5},
    {"_testxyz1_generateMatrix", (DL_FUNC) &_testxyz1_generateMatrix, 2},
    {"_testxyz1_go", (DL_FUNC) &_testxyz1_go, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_testxyz1(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
