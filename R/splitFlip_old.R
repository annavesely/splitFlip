#' @title Permutation-Based Multisplit (old)
#' @description This function computes permutation standardized scores for high-dimensional linear regression.
#' @usage splitFlip(X, Y, Q = 10, B = 200, target = NULL, exact = FALSE, varSel = targetLasso, varSelArgs = NULL, seed = NULL)
#' @param X numeric design matrix (including the intercept), where columns correspond to variables, and rows to observations.
#' @param Y numeric response vector.
#' @param Q numer of data splits.
#' @param B number of sign flips.
#' @param target maximum number of variables to be selected, between 1 and half the sample size.
#' If null, it is set to half the sample size.
#' @param exact logical, \code{TRUE} for the exact method, \code{FALSE} for the approximate method.
#' @param target maximum number of variables to be selected.
#' @param varSel a function to perform variable selection. It must have at least three arguments:
#' \code{X} (design matrix), \code{Y} (response vector) and \code{target} (maximum number of selected variables).
#' Additional arguments are passed through \code{varSelArgs}.
#' Return value is a numeric vector containing the indices of the selected variables.
#' @param varSelArgs named list of further arguments for \code{varSel}.
#' @param seed seed.
#' @details The data are iteratively split into two subsets of equal size for \code{Q} times.
#' For each split, the first subset is used to perform variable selection,
#' while the second is used to compute the effective scores for
#' each variable and \code{B} random sign flips (including the identity).
#' If a variable is not selected, its score is set to zero.
#' @details For each variable and each sign flip, the standardized score is defined as (an approximation of)
#' the sum of the effective scores over the \code{Q} splits, divided by its variance.
#' @return \code{splitFlip} returns a numeric matrix of standardized scores, where columns correspond to variables,
#' and rows to \code{B} random sign flips. The first flip is the identity.
#' @author Anna Vesely.
#' @examples
#' # generate linear regression data with 20 variables and 10 observations
#' res <- simData(prop=0.1, m=20, n=10, rho=0.5, type="toeplitz", SNR=5, seed=42)
#' X <- res$X # design matrix
#' Y <- res$Y # response vector
#' active <- res$active # indices of active variables
#'
#' # choose target as twice the number of active variables
#' target <- 2*length(active)
#'
#' # matrix of standardized scores for all variables (columns) and random sign flips (rows)
#' # using the approximate method with Lasso selection
#' G1 <- splitFlip(X, Y, target=target, seed=42)
#'
#' # maxT algorithm
#' maxT(G1, alpha=0.1)
#'
#' # matrix of standardized scores for all variables (columns) and random sign flips (rows)
#' # using the exact method with oracle selection
#' G2 <- splitFlip(X, Y, target=target, varSel=targetOracle, varSelArgs=list(toSel=active), seed=42)
#'
#' # maxT algorithm
#' maxT(G2, alpha=0.1)
#' @export


splitFlip_old <- function(X, Y, Q=10, B=200, target=NULL, exact=FALSE, varSel=targetLasso, varSelArgs=NULL, seed=NULL){

  if(!is.matrix(X) || !is.numeric(X) || !all(is.finite(X))){stop("X must be a matrix of finite numbers")}
  n <- nrow(X)
  m <- ncol(X)
  n0 <- floor(n/2)

  if(!is.vector(Y) || !is.numeric(Y) || !all(is.finite(Y))){stop("Y must be a vector of finite numbers")}
  if(!(length(Y)==n)){stop("Dimensions of X and Y are incompatible")}

  if(is.null(target)){target <- n0}
  if(!is.numeric(target) || !is.finite(target) || floor(target) <= 0 || floor(target) > n0){stop("target must be an integer between 1 and half the sample size")}
  target <- floor(target)

  if(!is.numeric(Q) || !is.finite(Q) || round(Q) <= 0){stop("Q must be a positive integer")}
  if(!is.numeric(B) || !is.finite(B) || round(B) <= 0){stop("B must be a positive integer")}
  Q <- round(Q)
  B <- round(B)

  if(!is.null(seed)){if(!is.numeric(seed) || !is.finite(seed)){stop("seed must be a finite integer")}}
  else{seed <- sample(seq(10^9), 1)}
  set.seed(round(seed))

  # select observations and variables
  sel <- getSplits(X, Y, Q, target, varSel, varSelArgs, seed)

  # maximum number of selected variables (useful if users add their own varSel function)
  smax <- max(sapply(sel$vars, length))
  if(smax > n0){stop("the number of selected variables should not exceed half the sample size")}

  # create all flips
  fl <- cbind(rep(1,n), replicate(B-1, 1-2*rbinom(n,1,0.5)))

  # create matrix of standardized scores
  G <- matrix(0, ncol=m, nrow=B)
  for(j in seq(m)){G[,j] <- jsplitFlip_old(j, X, Y, sel, fl, exact)}
  return(G)
}
