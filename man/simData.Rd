% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simData.R
\name{simData}
\alias{simData}
\title{Simulating Linear Regression Data}
\usage{
simData(prop, m, n, rho = 0, type = "equicorr", incrBeta = FALSE, SNR = 1, seed = NULL)
}
\arguments{
\item{prop}{proportion of active variables.}

\item{m}{number of variables.}

\item{n}{numer of observations.}

\item{rho}{level of equicorrelation between pairs of variables.}

\item{type}{type of covariance matrix among \code{equicorr} and \code{toeplitz}.}

\item{incrBeta}{logical, \code{TRUE} for increasing active coefficients (1,2,3,...),
\code{FALSE} for active coefficients all equal to 1.}

\item{SNR}{signal-to-noise-ratio (ratio of the variance of \code{beta X} to the error variance).}

\item{seed}{seed.}
}
\value{
\code{simData} returns a list containing the design matrix \code{X} (excluding the intercept),
the response vector \code{Y}, and the index vector of active variables \code{active}.
}
\description{
This function simulates a design matrix and a response vector.
}
\details{
The design matrix \code{X} contains \code{n} independent observations from a
MVN with mean 0 and covariance matrix \code{Sigma}. The term \code{Sigma(ij)} is given by \code{type}:
\itemize{
\item equicorrelation: 1 if \code{i=j}, and \code{rho} otherwise
\item Toeplitz: \code{rho^|i-j|}
}

A proportion \code{prop} of the coefficients are non-null, with values depending on \code{incrBeta}.
Then the response variable \code{Y} is equal to \code{X \%\% beta} plus an error term.
The standard deviation of this error term is such that the signal-to-noise ratio is \code{SNR}.
}
\examples{
# generate linear regression data with 20 variables and 10 observations
res <- simData(prop=0.1, m=20, n=10, rho=0.5, type="toeplitz", SNR=5, seed=42)
X <- res$X # design matrix
Y <- res$Y # response vector
active <- res$active # indices of active variables

# choose target as twice the number of active variables
target <- 2*length(active)

# matrix of standardized scores for all variables (columns) and random sign flips (rows)
# using the approximate method with Lasso selection
G <- splitFlip(X, Y, target=target, seed=42)

# maxT algorithm
maxT(G, alpha=0.1)
}
\author{
Anna Vesely.
}
