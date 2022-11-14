#' @title Variable Selection with Oracle
#' @description This function selects a given number of variables, always including those in a pre-specified set.
#' @usage selOracle(X, Y, target, toSel)
#' @param X numeric design matrix (excluding the intercept), where columns correspond to variables, and rows to observations.
#' @param Y numeric response vector.
#' @param target maximum number of variables to be selected.
#' @param toSel numeric vector containing the indices of the variables to be always selected.
#' @details An error message is returned if \code{target} is smaller than the length of \code{toSel}.
#' @return \code{selOracle} returns a numeric vector containing the indices of the selected variables.
#' Variables in \code{toSel} are always included.
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
#' # selection of at most target variables, including the active ones
#' selOracle(X, Y, target, active)
#' @export


selOracle <- function(X, Y, target, toSel){

  if(!is.matrix(X) || !is.numeric(X) || !all(is.finite(X))){stop("X must be a matrix of finite numbers")}
  if(!is.vector(Y) || !is.numeric(Y) || !all(is.finite(Y))){stop("Y must be a vector of finite numbers")}
  if(!(nrow(X)==length(Y))){stop("Dimensions of X and Y are incompatible")}

  m <- ncol(X)

  if(!is.vector(toSel) || !is.numeric(toSel)){stop("toSel must be a vector of finite integers")}
  if(!all(floor(toSel)==toSel)){stop("toSel must be a vector of finite integers")}
  if(!all(toSel >= 0) || !all(toSel <= m)){stop("toSel must contain indices between 1 and m")}
  toSel <- unique(toSel)

  if(!is.numeric(target) || !is.finite(target) || floor(target) <= 0){stop("target must be a positive integer")}
  target <- floor(target)

  others <- setdiff(seq(m), toSel)
  d <- target - length(toSel)
  if(d < 0){stop("target should be at least equal to the number of active variables")}
  d <- min(d, length(others))

  if(d == 0){
    sel <- sort(toSel)
  }else{
    sel <- sort(c(toSel, sample(others, d)))
  }
  return(sel)
}
