#' @title Variable Selection with Lasso
#' @description This function uses the Lasso to select a given number of variables in the framework of linear regression.
#' @usage targetLasso(X, Y, target = NULL)
#' @param X numeric design matrix (excluding the intercept), where columns correspond to variables, and rows to observations.
#' @param Y numeric response vector.
#' @param target number of variables to be selected at most. If null, it is set to \code{nrow(X)}.
#' @details The \code{lambda} parameter is chosen as the smallest parameter that selects at most \code{target} variables.
#' @details Notice that the number of selected variables will never exceed the number of observations.
#' @return \code{targetLasso} returns a numeric vector containing the indices of the selected variables.
#' @author Anna Vesely.
#' @importFrom glmnet glmnet
#' @examples
#' set.seed(42)
#' n <- 5 # observations
#' m <- 10 # variables
#' active <- c(1,2)
#' beta <- rep(0,m)
#' beta[active] <- 5
#' X <- matrix(rnorm(m*n), ncol=m)
#' Y <- rnorm(n=n, mean=X %*% beta)
#' target <- 4
#' targetLasso(X, Y, target)
#' @export


targetLasso <- function(X, Y, target=NULL){

  if(!is.matrix(X) || !is.numeric(X) || !all(is.finite(X))){stop("X must be a matrix of finite numbers")}
  if(!is.vector(Y) || !is.numeric(Y) || !all(is.finite(Y))){stop("Y must be a vector of finite numbers")}
  if(!(nrow(X)==length(Y))){stop("Dimensions of X and Y are incompatible")}

  if(is.null(target)){target <- nrow(X)}

  if(!is.numeric(target) || !is.finite(target)){stop("target must be a positive number")}
  target <- floor(target)
  if(target <= 0){stop("target must be a positive integer")}

  fit <- glmnet::glmnet(X, Y, family="gaussian", dfmax=target-1)
  L <- length(fit$lambda)
  sel <- coef(fit, s=fit$lambda[L])@i
  if(sel[1]==0){sel <- sel[-1]}
  return(sel)
}

