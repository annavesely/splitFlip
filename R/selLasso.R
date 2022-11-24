#' @title Variable Selection with Lasso
#' @description This function uses the Lasso to select variables in the framework of linear regression.
#' @usage selLasso(X, Y, target = NULL, nfolds = 10, ...)
#' @param X numeric design matrix (excluding the intercept), where columns correspond to variables, and rows to observations.
#' @param Y numeric response vector.
#' @param target maximum number of variables to be selected.
#' @param nfolds number of folds to be used in the cross-validation.
#' @param ... further arguments to be passed to \code{\link{hdi::lasso.cv}}.
#' @details If target is not null, the function uses the Lasso with suitable calibration of the \code{lambda} parameter to select at most \code{target} variables.
#' Otherwise, it uses the Lasso with 10-fold cross-validation.
#' @return \code{selLasso} returns a numeric vector containing the indices of the selected variables.
#' @author Anna Vesely.
#' @importFrom hdi lasso.firstq lasso.cv
#' @examples
#' # generate linear regression data with 20 variables and 10 observations
#' res <- simData(m1=2, m=20, n=10, rho=0.5, type="toeplitz", SNR=5, seed=42)
#' X <- res$X # design matrix
#' Y <- res$Y # response vector
#' active <- res$active # indices of active variables
#'
#' # choose target as twice the number of active variables
#' target <- 2*length(active)
#'
#' # Lasso selecting at most target variables
#' selLasso(X, Y, target)
#'
#' # Lasso with 10-fold cross-validation
#' selLasso(X, Y)
#' @importFrom hdi lasso.cv lasso.firstq
#' @export


selLasso <- function(X, Y, target=NULL, nfolds=10, ...){

  if(!is.matrix(X) || !is.numeric(Y) || !all(is.finite(X))){stop("X must be a matrix of finite numbers")}
  if(!is.vector(Y) || !is.numeric(Y) || !all(is.finite(Y))){stop("Y must be a vector of finite numbers")}
  if(!(nrow(X)==length(Y))){stop("Dimensions of X and Y are incompatible")}

  if(is.null(target)){
    sel <- hdi::lasso.cv(X,Y,nfolds,...)
    return(sel)
  }

  if(!is.numeric(target) || !is.finite(target) || floor(target) <= 0){stop("target must be a positive integer")}
  target <- floor(target)

  sel <- hdi::lasso.firstq(X,Y,target)
  return(sel)
}

