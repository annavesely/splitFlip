#' @title Simulating Linear Regression Data (2)
#' @description This function simulates a design matrix and a response vector.
#' @usage simData2(prop, m, n, rho = 0, alpha = 0.05, pw = 0.9, concordant = TRUE, seed = NULL)
#' @param prop proportion of active variables.
#' @param m number of variables.
#' @param n numer of observations.
#' @param rho level of equicorrelation between pairs of variables.
#' @param alpha significance level.
#' @param pw power of the t test.
#' @param concordant logical, \code{TRUE} for positive coefficients,
#' \code{FALSE} for coefficients with alternating signs.
#' @param seed seed.
#' @details The design matrix \code{X} contains \code{n} independent observations from a
#' MVN with mean 0 and equi-correlation \code{rho}.
#' @details A proportion \code{prop} of the coefficients are non-null.
#' The value of non-null coefficients is such that the one-sample t test with significance level \code{alpha},
#' using half the sample size, has power equal to \code{pw}. Then the response variable \code{Y}
#' is equal to \code{X %% beta} plus a standard normal error.
#' @return \code{simData} returns a list containing the design matrix \code{X} (excluding the intercept),
#' the response vector \code{Y}, and the index vector of active variables \code{active}.
#' @author Anna Vesely.
#' @examples
#' # generate linear regression data with 30 variables and 20 observations
#' res <- simData2(prop=0.05, m=30, n=20, seed=42)
#'
#' # choose target as twice the number of active variables
#' target <- 2*length(res$active)
#'
#' # matrix of standardized scores for all variables (columns) and random sign flips (rows)
#' # using the exact method with Lasso selection
#' G <- splitFlip(res$X, res$Y, target=target, exact=TRUE, seed=42)
#'
#' # maxT algorithm
#' maxT(G)
#' @export


simData2 <- function(prop, m, n, rho=0, alpha=0.05, pw=0.8, concordant=TRUE, seed=NULL){

  # check on prop
  if(!is.numeric(prop) || !is.finite(prop)){stop("prop must be a number in [0,1]")}
  if(prop < 0 || prop > 1){stop("prop must be a number in [0,1]")}

  # check on m and n
  if(!is.numeric(m) || !is.finite(m) || round(m) <= 0){stop("m must be a positive integer")}
  if(!is.numeric(n) || !is.finite(n) || round(n) <= 0){stop("n must be a positive integer")}
  m <- round(m)
  n <- round(n)

  # check on rho, alpha, pw
  if(!is.numeric(rho) || !is.finite(rho) || rho < -1 || rho > 1){stop("rho must be a number in [-1,1]")}
  if(!is.numeric(alpha) || !is.finite(alpha) || alpha <= 0 || alpha >= 1){stop("alpha must be a number in (0,1)")}
  if(!is.numeric(pw) || !is.finite(pw) || pw <= 0 || pw >= 1){stop("pw must be a number in (0,1)")}

  # check on seed
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
    beta[1:m1] <- c(rep(c(-signal, signal), floor(m1/2)), rep(-signal, ceiling(m1/2) - floor(m1/2)))
  }

  # X is a n x m matrix with the intercept and n independent observations from a MVN with mean 0 and equi-correlation rho
  X <- sqrt(1-rho) * matrix(rnorm(m*n), ncol=m) + sqrt(rho) * matrix(rep(rnorm(n), m), ncol=m)

  # generate Y from Gaussian model
  mu <- signal + X %*% beta
  Y <- rnorm(n=n, mean=X %*% beta)

  S <- seq(m1)

  out <- list("X"=X, "Y"=Y, "active"=S)
  return(out)
}


