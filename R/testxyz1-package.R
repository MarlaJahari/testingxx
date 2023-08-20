## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

## usethis namespace: start
#' @useDynLib testxyz1, .registration = TRUE
## usethis namespace: end
NULL

#given a list of cartesian products, returns all pairs
getpairs <- function(p) {
  mat<-matrix(0,0,2)
  for (i in seq_along(p)) {
    grid<-(expand.grid(p[[i]][[1]],p[[i]][[2]]))
    grid<- (as.matrix(grid))
    mat<- rbind(mat,grid)
    colnames(mat) <- NULL
  }
  return(t(mat))
}
