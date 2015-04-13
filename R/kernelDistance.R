#' Calculate the kernel distance
#'
#' This function calculates the kernel distance between an observation and the center of the SVDD.
#' @param point is the observation we are calculating the distance for.
#' @param data is training data used to train the SVDD.
#' @param alphas is the vector of alphas output from the SVDD training function.
#' @param gramMat is the gram matrix from the training data.
#' @param sigma is the scale parameter for the gaussian kernel.
#' @examples
#' trainingData <- dataTrain(10, 5)
#' sig <- 1.5
#' gm <- gram_mat(trainingData, sig)
#' svdd <- svddTrain(trainingData, gm, sig, 1)
#' alphaValues <- svdd$alpha
#' kernelDistance(trainingData[1,], trainingData, alphaValues, gm, sig)

kernelDistance <- function(point, data, alphas, gramMat, sigma){
  #calculate the distance for a single data point in the gaussian kernel space
  t1 <- gaussianKern(point, point, sigma)
  t2 <- -2*sum(sapply(1:length(alphas), function(m) alphas[m]*gaussianKern(point, data[m,], sigma)))
  t3 <- t(alphas) %*% gramMat %*% alphas
  sqrt(t1+t2+t3)
}