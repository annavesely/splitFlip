#' @title Variable Selection with Lasso
#' @description This function uses the Lasso to select a given number of variables in the framework of linear regression.
#' @usage targetLasso(X, Y, target = NULL)
#' @param X numeric design matrix (excluding the intercept), where columns correspond to variables, and rows to observations.
#' @param Y numeric response vector.
#' @param target maximum number of variables, i.e. columns of \code{X}, to be selected. If null, it is set to \code{nrow(X)}.
#' @details The \code{lambda} parameter is chosen as the smallest parameter that selects at most \code{target} variables.
#' @details Notice that the number of selected variables cannot exceed the number of observations.
#' @return \code{targetLasso} returns a numeric vector containing the indices of the selected variables.
#' @author Anna Vesely.
#' @importFrom glmnet glmnet
#' @examples
#' # generate linear regression data with 20 variables and 10 observations
#' res <- simData(prop=0.1, m=20, n=10, rho=0.5, type="toeplitz", SNR=4, seed=42)
#' X <- res$X # design matrix
#' Y <- res$Y # response vector
#' active <- res$active # indices of active variables
#'
#' # choose target as twice the number of active variables
#' target <- 2*length(active)
#'
#' # selection of at most target variables using the Lasso
#' targetLasso(X, Y, target)
#' @export


targetLasso <- function(X, Y, target=NULL){

  if(!is.matrix(X) || !is.numeric(X) || !all(is.finite(X))){stop("X must be a matrix of finite numbers")}
  if(!is.vector(Y) || !is.numeric(Y) || !all(is.finite(Y))){stop("Y must be a vector of finite numbers")}
  if(!(nrow(X)==length(Y))){stop("Dimensions of X and Y are incompatible")}

  if(is.null(target)){target <- nrow(X)}
  if(!is.numeric(target) || !is.finite(target) || floor(target) <= 0){stop("target must be a positive integer")}
  target <- floor(target)

  fit <- glmnet::glmnet(X, Y, family="gaussian", dfmax=target)
  L <- length(fit$lambda[fit$df <= target])
  sel <- coef(fit, s=fit$lambda[L])@i
  sel <- sel[sel != 0]
  return(sel)
}

