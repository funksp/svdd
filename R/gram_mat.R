#' Gram Matrix
#'
#' This function calculates the NxN gram matrix -- Kernel(X, X)
#' @param mydat is the training data.
#' @param sigma is the scale parameter for use in the gaussian kernel.
#' @examples
#' trainingData <- dataTrain(10, 5)
#' gram_mat(trainingData, 1.5)

gram_mat <- function(mydat, sigma){
  N <- dim(mydat)[1]
  if (!is.matrix(mydat)) mydat <- as.matrix(mydat) #change class of mydat to matrix
  gram_matrix <- matrix(0, N, N)
  for(i in 1:N){
    for(k in 1:N){
      gram_matrix[i,k] <- gaussianKern(mydat[i,], mydat[k,], sigma=sigma)
    }
  }
  gram_matrix
}