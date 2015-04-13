#' Train the Support Vector Data Descriptor (SVDD)
#'
#' This function trains the SVDD model.
#' @param X is the training data.
#' @param Gram_Matrix is the gram matrix created by the gram_mat function.
#' @param sigma is the scale parameter for the gaussian kernel.
#' @param C1 is the cost parameter for the postive examples.
#' @param C2 is the cost parameter for the negative examples.
#' @param negativeProportion proportion of negative examples present in the training data.
#' @examples
#' trainingData <- dataTrain(10, 5)
#' sig <- 1.5
#' gm <- gram_mat(trainingData, sig)
#' svddTrain(trainingData, gm, sig, 1)

svddTrain <- function(X, Gram_Matrix, sigma, C1, C2=0, negativeProportion=0){
  if (!is.matrix(X)) X <- as.matrix(X)
  N <- dim(X)[1]
  numNegative <- round(negativeProportion*N) #number of negative rows in training data
  numPositive <- N-numNegative #number of positive rows in training data
  d <- make.d.vec(X, sigma)
  D <- gram_mat(X, sigma)
  D <- 2*D
  D <- D + diag(dim(D)[1])*1e-12
  
  # create b, the first and second row makes sure alphas sum 
  #   to 1, the others gaurantee they are greater than 0:
  bv <- c(1, 
          rep(0, N), 
          rep(-C1, numPositive), 
          rep(-C2, numNegative))
  
  #Initialize the designed A matrix to go along with bv:
  A  <- cbind(rep(1, N), diag(N), -diag(N))
  alpha <- solve.QP(D, d, A, bv, meq=1)$solution #the alphas
  non_zero_alphas <- alpha[round(alpha, digits=4) > 0]
  locations <- which(round(alpha, digits=4) > 0)
  support_vectors <- X[locations,]
  num_SVs <- length(locations)
  center <- t(alpha) %*% X
  return(list(num_SVs=num_SVs, 
              locations=locations, 
              alpha=alpha, 
              nza=non_zero_alphas, 
              sv=support_vectors, 
              ctr=center))
}