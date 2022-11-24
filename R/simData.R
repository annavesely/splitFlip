#' @title Simulating Linear Regression Data
#' @description This function simulates a design matrix and a response vector.
#' @usage simData(m1, m, n, rho = 0, type = "equicorr", incrBeta = FALSE, SNR = 1, seed = NULL)
#' @param m1 number of active variables.
#' @param m total number of variables.
#' @param n number of observations.
#' @param rho correlation parameter.
#' @param type type of covariance matrix among \code{equicorr} and \code{toeplitz}.
#' @param incrBeta logical, \code{TRUE} for increasing active coefficients (1,2,3,...),
#' \code{FALSE} for active coefficients all equal to 1.
#' @param SNR signal-to-noise-ratio (ratio between the variances of \code{X beta} and the error term).
#' @param seed seed.
#' @details The design matrix \code{X} contains \code{n} independent observations from a
#' MVN with mean 0 and covariance matrix \code{Sigma}. The term \code{Sigma(ij)} is given by \code{type}:
#' \itemize{
#' \item equicorrelation: 1 if \code{i=j}, and \code{rho} otherwise
#' \item Toeplitz: \code{rho^|i-j|}
#' }
#' @details A number \code{m1} of the coefficients are non-null, with values depending on \code{incrBeta}.
#' Then the response variable \code{Y} is equal to \code{X beta} plus an error term.
#' The standard deviation of this error term is such that the signal-to-noise ratio is \code{SNR}.
#' @return \code{simData} returns a list containing the design matrix \code{X} (not including the intercept),
#' the response vector \code{Y}, and the index vector of active variables \code{active}.
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
#' # standardized scores using the approximate method with Lasso selection of target variables
#' G <- splitFlip(X, Y, target=target, seed=42)
#'
#' # maxT algorithm
#' maxT(G, alpha=0.1)
#' @importFrom mvtnorm rmvnorm
#' @export



simData <- function(m1, m, n, rho=0, type="equicorr", incrBeta=FALSE, SNR=1, seed=NULL){

  if(!is.numeric(m1) || !is.finite(m1)){stop("m1 must be a number in [0,m]")}
  if(m1 < 0 || m1 > m){stop("m1 must be a number in [0,m]")}

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

  if(rho == 0){
    X <- matrix(rnorm(m*n), ncol=m)
  }else if(type == "equicorr"){
    X <- sqrt(1-rho) * matrix(rnorm(m*n), ncol=m) + sqrt(rho) * matrix(rep(rnorm(n), m), ncol=m)
  }else{
    r <- rho^(0:(m-1))
    Sigma <- stats::toeplitz(r)
    X <- mvtnorm::rmvnorm(n, sigma=Sigma)
  }

  active <- sample(seq(m), m1)
  beta <- rep(0, m)
  if(incrBeta){
    beta[active] <- seq(m1)
  }else{
    beta[active] <- rep(1,m1)
  }
  active <- sort(active)

  mu <- X %*% beta
  mu_var <- var(mu) * (n-1) / n
  serr <- sqrt(mu_var/SNR) # sd of the error term

  Y <- rnorm(n=n, mean=mu, sd=serr)

  out <- list("X"=X, "Y"=Y, "active"=active)
  return(out)
}
