#' Fit a Ridge Regression Model
#'
#' @description This function passes parameters to the ridge_reg function.
#' @param form A formula
#' @param lambda A penalty value 
#' @param d A data.frame
#' @return An lm object
#' @importFrom stats model.matrix 
#' @examples
#' @export


ridge_reg <- function(form, lambda, d){
  rownames(data) <- NULL
  m <- model.matrix(form, d)
  y <- matrix(d[,as.character(form)[2]], ncol = 1)
  y <- y[as.numeric(rownames(m)),, drop = FALSE]
  
  svd_obj <- svd(m)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d
  
  D <- diag(svals / (svals^2 + lambda))
  beta <- V %*% D %*% t(U) %*% y
  rownames(beta) <- colnames(m)
  ret <- list(coefficients = beta, lambda = lambda, form = form)
  class(ret) <- "ridge_reg"
  return(ret)
}
