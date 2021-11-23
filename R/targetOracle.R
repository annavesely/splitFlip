#' @title Variable Selection with Oracle
#' @description This function selects a given number of variables, always including those in a pre-specified set.
#' @usage targetOracle(X, Y, target = NULL, m, active)
#' @param X numeric design matrix (excluding the intercept), where columns correspond to variables, and rows to observations.
#' @param Y numeric response vector.
#' @param target number of variables to be selected at most. If null, it is set to \code{nrow(X)}.
#' @param m total number of variables.
#' @param active numeric vector containing the indices of the variables to be always selected.
#' @details An error message is returned if \code{target} is smaller than the length of \code{active}.
#' @return \code{targetOracle} returns a numeric vector containing the indices of the selected variables.
#' Variables in \code{active} are always included.
#' @author Anna Vesely.
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
#' targetOracle(X, Y, target, m, active)
#' @export


targetOracle <- function(X, Y, target, m, active){

  if(!is.matrix(X) || !is.numeric(X) || !all(is.finite(X))){stop("X must be a matrix of finite numbers")}
  if(!is.vector(Y) || !is.numeric(Y) || !all(is.finite(Y))){stop("Y must be a vector of finite numbers")}
  if(!(nrow(X)==length(Y))){stop("Dimensions of X and Y are incompatible")}

  if(!is.numeric(m) || !is.finite(m)){stop("m must be a positive number")}
  m <- round(m)
  if(m <= 0){stop("m must be a positive integer")}

  if(!is.vector(active) || !is.numeric(active)){stop("active must be a vector of finite integers")}
  if(!all(floor(active)==active)){stop("active must be a vector of finite integers")}
  if(!all(active >= 0) || !all(active <= m)){stop("active must contain indices between 1 and m")}
  active <- unique(active)

  if(is.null(target)){target <- nrow(X)}

  if(!is.numeric(target) || !is.finite(target) || target <= 0){stop("target must be a positive number")}
  target <- round(target)
  if(target <= 0){stop("target must be a positive integer")}

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
