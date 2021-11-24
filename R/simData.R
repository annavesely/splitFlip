#' @title Simulating Linear Regression Data
#' @description This function simulates a design matrix and a response vector.
#' @usage simData(prop, m, n, rho = 0, alpha = 0.05, pw = 0.9, concordant = TRUE, seed = NULL)
#' @param prop proportion of active variables.
#' @param m number of variables.
#' @param n numer of observations.
#' @param rho level of equicorrelation between pairs of variables.
#' @param alpha significance level.
#' @param pw power of the t test.
#' @param concordant logical, \code{TRUE} for positive coefficients,
#' \code{FALSE} for coefficients with alternating signs.
#' @param seed seed.
#' @details The covariate matrix \code{X} contains the intercept and \code{n} independent observations from a
#' MVN with mean 0 and equi-correlation \code{rho}.
#' @details A proportion \code{prop} of the coefficients are non-null. Their value is such that
#' the one-sample t test with significance level \code{alpha},
#' using half the sample size, has power equal to \code{pw}.
#' @return \code{simData} returns a list containing the covariate matrix \code{X} (including the intercept),
#' the response vector \code{Y}, and the index vector of active variables \code{active.}
#' @author Anna Vesely.
#' @examples
#' # generate linear regression data with 20 variables and 10 observations
#' res <- simData(prop=0.1, m=20, n=10, seed=42)
#'
#' # choose target as twice the number of active variables
#' target <- 2*length(res$active)
#'
#' # matrix of standardized scores for all variables (columns) and random sign flips (rows)
#' # using the exact method with 10 data splits and Lasso selection
#' G1 <- splitFlip(res$X, res$Y, target=target, B=6, exact=TRUE)
#' round(G1,2)
#'
#' # matrix of standardized scores for all variables (columns) and random sign flips (rows)
#' # using the approximate method with 5 data splits and oracle selection
#' G2 <- splitFlip(res$X, res$Y, target=target, Q=5, B=6,
#'                varSel=targetOracle, varSelArgs=list(m=ncol(res$X), active=res$active))
#' round(G2,2)
#' @export


simData <- function(prop, m, n, rho=0, alpha=0.05, pw=0.9, concordant=TRUE, seed=NULL){

  if(!is.numeric(prop) || !is.finite(prop)){stop("prop must be a number in [0,1]")}
  if(prop < 0 || prop > 1){stop("prop must be a number in [0,1]")}

  # check on m, B, n
  if(!is.numeric(m) || !is.finite(m) || round(m) <= 0){stop("m must be a positive integer")}
  if(!is.numeric(n) || !is.finite(n) || round(n) <= 0){stop("n must be a positive integer")}
  m <- round(m)
  n <- round(n)

  # check on rho, alpha, pw
  if(!is.numeric(rho) || !is.finite(rho) || rho < -1 || rho > 1){stop("rho must be a number in [-1,1]")}
  if(!is.numeric(alpha) || !is.finite(alpha) || alpha <= 0 || alpha >= 1){stop("alpha must be a number in (0,1)")}
  if(!is.numeric(pw) || !is.finite(pw) || pw <= 0 || pw >= 1){stop("pw must be a number in (0,1)")}

  if(!is.null(seed)){if(!is.numeric(seed) || !is.finite(seed)){stop("seed must be a finite integer")}}
  else{seed <- sample(seq(10^9), 1)}
  set.seed(round(seed))

  m1 <- ceiling(m * prop)
  n0 <- floor(n/2)
  signal <- power.t.test(power=pw, n=n0, sig.level=alpha, type="one.sample", alternative="two.sided", sd=1)$delta

  beta <- rep(0,m)
  if(concordant){
    beta[1:m1] <- rep(signal, m1)
  }else{
    beta[1:m1] <- c(rep(c(signal, -signal), floor(m1/2)), rep(signal, ceiling(m1/2) - floor(m1/2)))
  }

  # X is a n x m matrix with the intercept and n independent observations from a MVN with mean 0 and equi-correlation rho
  X <- matrix(1, ncol=m, nrow=n)
  X[,-1] <- sqrt(1-rho) * matrix(rnorm((m-1)*n), ncol=m-1) + sqrt(rho) * matrix(rep(rnorm(n), m-1), ncol=m-1)

  # generate Y from Gaussian model
  Y <- rnorm(n=n, mean=X %*% beta)

  S <- seq(m1)

  out <- list("X"=X, "Y"=Y, "active"=S)
  return(out)
}


