
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats model.matrix
#' @export
linear_model <- function(formula, data) {
  coef<-list()
  x_var<-all.vars(formula)
  X<-model.matrix(formula, data)
  Y<-data[,x_var[1]]
  
  coef$coefficients=qr.coef(qr(X),Y)
  class(coef)="lm"
  return(coef)
}
