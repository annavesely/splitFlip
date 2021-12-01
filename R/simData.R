#' @title Simulating Linear Regression Data
#' @description This function simulates a design matrix and a response vector.
#' @usage simData(prop, m, n, rho = 0, type = "equicorr", incrBeta = FALSE, SNR = 1, seed = NULL)
#' @param prop proportion of active variables.
#' @param m number of variables.
#' @param n numer of observations.
#' @param rho level of equicorrelation between pairs of variables.
#' @param type type of covariance matrix among \code{equicorr} and \code{toeplitz}.
#' @param incrBeta logical, \code{TRUE} for increasing active coefficients (1,2,3,...),
#' \code{FALSE} for active coefficients all equal to 1.
#' @param SNR signal-to-noise-ratio (ratio of the variance of \code{beta X} to the error variance).
#' @param seed seed.
#' @details The design matrix \code{X} contains \code{n} independent observations from a
#' MVN with mean 0 and covariance matrix \code{Sigma}. The term \code{Sigma(ij)} is given by \code{type}:
#' \itemize{
#' \item equicorrelation: 1 if \code{i=j}, and \code{rho} otherwise
#' \item Toeplitz: \code{rho^|i-j|}
#' }
#' @details A proportion \code{prop} of the coefficients are non-null, with values depending on \code{incrBeta}.
#' Then the response variable \code{Y} is equal to \code{X %% beta} plus an error term.
#' The standard deviation of this error term is such that the signal-to-noise ratio is \code{SNR}.
#' @return \code{simData} returns a list containing the design matrix \code{X} (excluding the intercept),
#' the response vector \code{Y}, and the index vector of active variables \code{active}.
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
#' G <- splitFlip(X, Y, target=target, seed=42)
#'
#' # maxT algorithm
#' maxT(G, alpha=0.1)
#' @export



simData <- function(prop, m, n, rho=0, type="equicorr", incrBeta=FALSE, SNR=1, seed=NULL){

  if(!is.numeric(prop) || !is.finite(prop)){stop("prop must be a number in [0,1]")}
  if(prop < 0 || prop > 1){stop("prop must be a number in [0,1]")}

  # check on m and n
  if(!is.numeric(m) || !is.finite(m) || round(m) <= 0){stop("m must be a positive integer")}
  if(!is.numeric(n) || !is.finite(n) || round(n) <= 0){stop("n must be a positive integer")}
  m <- round(m)
  n <- round(n)

  # check on rho, type and SNR
  if(!is.numeric(rho) || !is.finite(rho) || rho < -1 || rho > 1){stop("rho must be a number in [-1,1]")}
  type <- match.arg(tolower(type), c("equicorr", "toeplitz"))
  if(!is.numeric(SNR) || !is.finite(SNR) || SNR <= 0){stop("SNR must be a postive number")}

  # check on seed
  if(!is.null(seed)){if(!is.numeric(seed) || !is.finite(seed)){stop("seed must be a finite integer")}}
  else{seed <- sample(seq(10^9), 1)}
  set.seed(round(seed))

  if(type == "equicorr"){
    X <- sqrt(1-rho) * matrix(rnorm(m*n), ncol=m) + sqrt(rho) * matrix(rep(rnorm(n), m), ncol=m)
  }else{
    r <- rho^(0:(m-1))
    Sigma <- stats::toeplitz(r)
    X <- mvtnorm::rmvnorm(n, sigma=Sigma)
  }

  m1 <- ceiling(m * prop)
  active <- seq(m1)
  beta <- rep(0,m)
  beta[active] <- ifelse(incrBeta, active, rep(1,m1))

  mu <- X %*% beta
  serr <- sqrt(var(mu)/SNR) # sd of the error term

  Y <- rnorm(n=n, mean=mu, sd=serr)

  out <- list("X"=X, "Y"=Y, "active"=active)
  return(out)
}
