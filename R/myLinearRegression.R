#' Linear Regression Analysis
#'
#' This is a function that run linear regression analysis on the Y on X, and paired graphs of variables.
#'
#' @param Y A vector of outcomes.
#' @param X A Matrix of covariates.
#' @param sub A List of subjects.
#' @return A list contains coefficients and p-values.
#' @export
#' @examples
#' myLinearRegression(myData[,1],myData[,2:5],1:10)
#' myLinearRegression(myData[,1],myData[,2:3],1:20)

myLinearRegression<-function(Y,X,sub){
  if (ncol(X)>5){
    print("Too many variables to plot")
  }

  if (ncol(X)<=5){
    model_lm<-lm(Y[sub]~X[sub,ncol(X)])
  }

  return(list("Coefficient"=model_lm$coefficients,
              "P-Value"=summary(model_lm)$coefficients[,4],
              GGally::ggpairs(X)))
}
