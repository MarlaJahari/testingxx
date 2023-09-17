## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

## usethis namespace: start
#' @useDynLib testxyz1, .registration = TRUE
## usethis namespace: end
NULL

getpairs0 <- function(p) {
  mat<-matrix(0,0,2)
  for (i in seq_along(p)) {
    grid<-(expandGrid(p[[i]][[1]],p[[i]][[2]]))
    grid<- (as.matrix(grid))
    mat<- rbind(mat,grid)
    colnames(mat) <- NULL
  }
  return(t(mat))
}

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

getpairs2 <- function(p) {
  num_pairs<-length(p)
  total_combinations<-sum(sapply(p, function(x) length(x[[1]]) * length(x[[2]])))

  mat<-matrix(0, total_combinations, 2)
  idx<-1

  for (i in seq_along(p)) {
    pair<-p[[i]]
    len_1<-length(pair[[1]])
    len_2<-length(pair[[2]])

    grid<-expand.grid(pair[[1]], pair[[2]])
    mat[idx:(idx + len_1 * len_2 - 1), ] <- as.matrix(grid)

    idx<-idx + len_1 * len_2
  }

  return(mat)
}

getpairs3 <- function(p) {
  grids <- lapply(p, function(x) expand.grid(x[[1]], x[[2]]))
  non_empty_grids <- Filter(function(x) !is.null(x), grids)

  if (length(non_empty_grids)==0) {
    return(matrix(0, 0, 2))
  }

  mat<- do.call(rbind, non_empty_grids)
  colnames(mat) <- NULL
  return(mat)
}




getunion<-function(x)
  {mat1 <- matrix(nrow = 2)
 for (i in 1:length(x)) {
  mat1<-cbind(mat1,getpairs(x[[i]]))
 }
  return(mat1)}

#mat1 <- matrix(nrow = 2)
#mat2 <- matrix(7:12, ncol = 2)

# transpose the matrix to have points in rows instead of columns
#transposed_matrix<- t(mat1[,-1])

# calculate the size of the plotting character based on the number of points
#num_points<- ncol(transposed_matrix)
#pch_size<- 10 +num_points*5

# talculate the alpha transparency based on the number of points
#alpha_value<- 1/(num_points+1)  #  for more transparency

# plot the points with varying sizes and alpha transparency
#plot(transposed_matrix, type= "p", pch= 19, col= rgb(0,0,1, alpha=alpha_value),
#     xlim=c(min(transposed_matrix[,1]), max(transposed_matrix[, 1])),
#     ylim=c(min(transposed_matrix[,2]), max(transposed_matrix[, 2])),
#     xlab="X Coordinate", ylab ="Y Coordinate", main ="Plot of Points")

# add labels to the points
#text(transposed_matrix[, 1], transposed_matrix[, 2], labels = 1:ncol(transposed_matrix), pos = 3)


# create two example matrices with the same number of rows
#mat1 <- matrix(nrow = 2)
#mat2 <- matrix(7:12, ncol = 2)

# concatenate the matrices by columns
#concatenated_mat <- cbind(mat1, mat2)

# Print the concatenated matrix
#print(concatenated_mat)
