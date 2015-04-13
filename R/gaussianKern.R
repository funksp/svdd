#' Gaussian Kernel
#'
#' This function calculates the gaussian kernel
#' @param x a single observation (vector).
#' @param y a single observation (vector).
#' @param sigma the scaling parameter for the gaussian kernel
#' @examples
#' gaussianKern(c(2, 5), c(3, 2), sigma=1.5)

gaussianKern <- function(x, y, sigma){
  exp(-(t(x-y)%*%(x-y))/(2*sigma^2))
}