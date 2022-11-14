#' @title Variable Selection with Highest Correlation
#' @description This function selects a given number of variables in the framework of linear regression.
#' The selected variables are those having the highest correlation (in absolute value) with the response.
#' @usage selCorrelation(X, Y, target)
#' @param X numeric design matrix (excluding the intercept), where columns correspond to variables, and rows to observations.
#' @param Y numeric response vector.
#' @param target maximum number of variables to be selected.
#' @return \code{selCorrelation} returns a numeric vector containing the indices of the selected variables.
#' @author Anna Vesely.
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
#' # selection of at most target variables using highest correlations
#' selCorrelation(X, Y, target)
#' @export


selCorrelation <- function(X, Y, target){

  if(!is.matrix(X) || !is.numeric(X) || !all(is.finite(X))){stop("X must be a matrix of finite numbers")}
  if(!is.vector(Y) || !is.numeric(Y) || !all(is.finite(Y))){stop("Y must be a vector of finite numbers")}
  if(!(nrow(X)==length(Y))){stop("Dimensions of X and Y are incompatible")}

  if(!is.numeric(target) || !is.finite(target) || floor(target) <= 0){stop("target must be a positive integer")}
  target <- floor(target)
  target <- min(target, ncol(X))

  c <- abs(cor(X,Y))
  sel <- order(c, decreasing=TRUE)[seq(target)]
  sel <- sort(sel)
  return(sel)
}

