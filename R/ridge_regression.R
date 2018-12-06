#' Fit a ridge regression model
#'
#' @description This function passes parameters to the ridge regression function.
#' @param formula a formula
#' @param lambda a hyper parameter
#' @param data a data.frame
#' @return An ridge regression object
#' @export

#build ridge function
ridge_reg<-function(formula, lambda, data){
  rownames(data) = NULL
  m<-model.matrix(formula, data)
  y<-matrix(data[,as.character(formula)[2]],ncol=1)
  y<-y[as.numeric(rownames(m)),,drop=FALSE]
  
  #Fit via svd
  svd_obj<-svd(m)
  U<-svd_obj$u
  V<-svd_obj$v
  svals<-svd_obj$d
  
  D<-diag(svals/(svals^2 +lambda))
  beta<-V %*% D %*% t(U) %*% y
  rownames(beta) = colnames(m)
  ret<- list(coefficients = beta, lambda=lambda,formula=formula)
  class(ret)<-"ridge_reg"
  ret
}#function end
