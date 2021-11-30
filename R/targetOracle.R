#' @title Variable Selection with Oracle
#' @description This function selects a given number of variables, always including those in a pre-specified set.
#' @usage targetOracle(X, Y, target = NULL, toSel)
#' @param X numeric design matrix (excluding the intercept), where columns correspond to variables, and rows to observations.
#' @param Y numeric response vector.
#' @param targetmaximum number of variables, i.e. columns of \code{X}, to be selected. If null, it is set to \code{nrow(X)}.
#' @param toSel numeric vector containing the indices of the variables to be always selected.
#' @details An error message is returned if \code{target} is smaller than the length of \code{toSel}.
#' @return \code{targetOracle} returns a numeric vector containing the indices of the selected variables.
#' Variables in \code{toSel} are always included.
#' @author Anna Vesely.
#' @examples
#' # generate linear regression data with 20 variables and 10 observations
#' res <- simData(prop=0.1, m=20, n=10, rho=0.5, type="toeplitz", seed=42)
#' X <- res$X # design matrix
#' Y <- res$Y # response vector
#' active <- res$active # indices of active variables
#'
#' # choose target as twice the number of active variables
#' target <- 2*length(active)
#'
#' # selection of at most target variables, including the active ones
#' targetOracle(X, Y, target, active)
#' @export


targetOracle <- function(X, Y, target, active){

  if(!is.matrix(X) || !is.numeric(X) || !all(is.finite(X))){stop("X must be a matrix of finite numbers")}
  if(!is.vector(Y) || !is.numeric(Y) || !all(is.finite(Y))){stop("Y must be a vector of finite numbers")}
  if(!(nrow(X)==length(Y))){stop("Dimensions of X and Y are incompatible")}

  m <- ncol(X)

  if(!is.vector(active) || !is.numeric(active)){stop("active must be a vector of finite integers")}
  if(!all(floor(active)==active)){stop("active must be a vector of finite integers")}
  if(!all(active >= 0) || !all(active <= m)){stop("active must contain indices between 1 and m")}
  active <- unique(active)

  if(is.null(target)){target <- nrow(X)}
  if(!is.numeric(target) || !is.finite(target) || floor(target) <= 0){stop("target must be a positive integer")}
  target <- floor(target)

  inactive <- setdiff(seq(m), active)
  d <- target - length(active)
  if(d < 0){stop("target should be at least equal to the number of active variables")}
  d <- min(d, length(inactive))

  if(d == 0){
    sel <- active
  }else{
    sel <- sort(c(active, sample(inactive, d)))
  }
  return(sel)
}
