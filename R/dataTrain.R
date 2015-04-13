#' Data Generation
#'
#' This function simulates some data containing postive and negative examples.
#' @param n the total number of rows in the data.
#' @param p the total number of columns in the data
#' @param negativeProportion the proportion of 'negative' examples in the data. Defaults to 0.
#' @examples
#' dataTrain(10, 5, 0.2)


dataTrain <- function(n, p, negativeProportion=0){
  numNegative <- round(negativeProportion*n)
  numPositive <- n-numNegative
  positiveMean <- rnorm(p, mean=4, sd=1)
  negativeMean <- rnorm(p, mean=-4, sd=1)
  Mat <- matrix(0, p, p)
  for(i in 1:p){
    for(j in 1:p){
      if(i==j){Mat[i, j] <- 2}
      else{
        Mat[i, j] <- 0.1 ^ abs(i-j)}
    }
  }
  sigma <- Mat
  positiveData <- mvrnorm(numPositive, positiveMean, sigma)
  if(numNegative > 0) {
    negativeData <- mvrnorm(numNegative, negativeMean, sigma)
    return(rbind(positiveData, negativeData))
  }
  else return(positiveData)
}