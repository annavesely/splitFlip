#' @title Residual Matrix
#' @description Internal function. It computes the residual matrix in the framework of linear regression.
#' @usage residualMatrix(Z)
#' @param Z numeric matrix of covariates, where columns correspond to variables, and rows to observations.
#' @details The residual matrix is \code{I - H}, where \code{I} is the identity and \code{H} is the projection matrix.
#' @details If the number of columns of \code{Z} is zero, the residual matrix is the identity.
#' @return \code{residualMatrix} returns the residual matrix for \code{Z}.
#' @author Anna Vesely.
#' @noRd


residualMatrix <- function(Z){
  R <- diag(nrow(Z))
  if(ncol(Z) > 0){
    R <- R - (Z %*% solve(t(Z) %*% Z) %*% t(Z))
  }
  return(R)
}
