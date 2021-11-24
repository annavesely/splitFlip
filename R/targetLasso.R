#' @title Variable Selection with Lasso
#' @description This function uses the Lasso to select a given number of variables in the framework of linear regression.
#' @usage targetLasso(X, Y, target = NULL)
#' @param X numeric design matrix (including the intercept), where columns correspond to variables, and rows to observations.
#' @param Y numeric response vector.
#' @param target maximum number of variables to be selected. If null, it is set to \code{nrow(X)}.
#' @details The \code{lambda} parameter is chosen as the smallest parameter that selects at most \code{target} variables.
#' @details Notice that the number of selected variables cannot exceed the number of observations.
#' @return \code{targetLasso} returns a numeric vector containing the indices of the selected variables.
#' @author Anna Vesely.
#' @importFrom glmnet glmnet
#' @examples
#' # generate linear regression data with 20 variables and 10 observations
#' res <- simData(prop=0.1, m=20, n=10, seed=42)
#'
#' # choose target as twice the number of active variables
#' target <- 2*length(res$active)
#'
#' # selection of at most target variables using the Lasso
#' targetLasso(res$X, res$Y, target)
#' @export


targetLasso <- function(X, Y, target=NULL){

  if(!is.matrix(X) || !is.numeric(X) || !all(is.finite(X))){stop("X must be a matrix of finite numbers")}
  if(!is.vector(Y) || !is.numeric(Y) || !all(is.finite(Y))){stop("Y must be a vector of finite numbers")}
  if(!(nrow(X)==length(Y))){stop("Dimensions of X and Y are incompatible")}

  X <- X[,-1]

  if(is.null(target)){target <- nrow(X)}
  if(!is.numeric(target) || !is.finite(target) || floor(target) <= 0){stop("target must be a positive integer")}
  target <- floor(target)

  fit <- glmnet::glmnet(X, Y, family="gaussian", dfmax=target-2)
  L <- length(fit$lambda)
  sel <- coef(fit, s=fit$lambda[L])@i + 1
  return(sel)
}

