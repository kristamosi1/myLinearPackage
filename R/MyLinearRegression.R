#' My Linear Regression
#'
#' @param Y  A vector of outcomes
#' @param X  A matrix of covariates
#' @param sub a list of  subjects (i.e.  a set of integers corresponding to rows in X)
#'
#' @return Returns the coefficients of the linear regression of X and Y and the corresponding p-values
#' @export
#'
#' @examples
#'
#'
myLinearRegression <- function(Y,X,sub){
  outputs <- list()
  Y <- subset(Y, seq(1,length(Y), by = 1)%in%sub)
  X <- subset(X, seq(1,nrow(X), by =1)%in%sub)

  fit<- lm(Y~ X)

  if( ncol(X) < 5) {
    outputs[["Plot"]] <- GGally::ggpairs(data.frame(X))

  }else{

    # code for the plot
    outputs[["Plot"]] <- ggplot2::ggplot() + ggplot2::geom_text(aes(x =0, y=0, label = "Too many variables to plot"), size = 8)

  }
  outputs[["Coefficients"]] <- fit$coefficients
  outputs[["P-value"]] <- summary(fit)$coefficients[,4]
  return(outputs)
}
