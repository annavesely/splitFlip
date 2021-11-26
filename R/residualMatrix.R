#' @title Residual Matrix
#' @description Internal function. It computes the residual matrix in the framework of linear regression.
#' @usage residualMatrix(j, X, obs, vars)
#' @param j index of the variable to be considered.
#' @param X numeric design matrix (excluding the intercept), where columns correspond to variables, and rows to observations.
#' @param obs list of observations for different splits, as returned by \code{getSplits}.
#' @param vars list of variables for different splits, as returned by \code{getSplits}.
#' @details The residual matrix has non-zero values only corresponding to observations in \code{obs},
#' where it is computed as the difference between the identity matrix and a projection matrix \code{H}.
#' The matrix \code{H} is computed by taking from \code{X} only \code{obs} and \code{vars},
#' and then adding the intercept.
#' @return \code{residualMatrix} returns the residual matrix for \code{j},
#' computed taking only observations in \code{obs} and variables in \code{vars}.
#' @author Anna Vesely.
#' @noRd


residualMatrix <- function(j, X, obs, vars){

  n <- nrow(X)
  R <- matrix(0, ncol=n, nrow=n)

  n0 <- length(obs)
  intercept <- rep(1, n0)
  Xsel <- as.matrix(X[obs, vars])
  Z <- cbind(intercept, Xsel)

  R[obs,obs] <- diag(n0) - (Z %*% solve(t(Z) %*% Z) %*% t(Z))
  return(R)
}
