#' d vector 
#'
#' This is not to be called by the user. This function is for training the SVDD and is necessary for the quadratic programming problem.
#' @param mydat is the training data.
#' @param sigma is the scale parameter for use in the gaussian kernel.

make.d.vec <- function(mydat, sigma){
  #creates the d vector for quadprog
  # Note that because of the structure of our "D" matrix,
  #   the reverse of what we normally use for a d vector 
  #   is concatenated onto itself
  d <- sapply(1:dim(mydat)[1], function(m) gaussianKern(mydat[m,], mydat[m,], sigma=sigma))
  d
}