#' @title Single-Step maxT
#' @description This function applies the single-step maxT algorithm.
#' @usage maxT(G, alpha = 0.05)
#' @param G numeric matrix of statistics, where columns correspond to variables, and rows to data transformations.
#' The first transformation is the identity.
#' @param alpha significance level.
#' @return \code{maxT} returns a list containing the p-values for each variable, and the indices of rejected variables.
#' @author Anna Vesely.
#' @examples
#' # generate linear regression data with 20 variables and 10 observations
#' res <- simData(prop=0.1, m=20, n=10, rho=0.5, type="toeplitz", SNR=4, seed=42)
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


maxT <- function(G, alpha=0.05){

  if(!is.matrix(G) || !is.numeric(G) || !all(is.finite(G))){stop("G must be a matrix of finite numbers")}
  if(!is.numeric(alpha) || !is.finite(alpha) || alpha <= 0 || alpha >= 1){stop("alpha must be a number in (0,1)")}

  m <- ncol(G)
  B <- nrow(G)
  o <- order(abs(G[1,]), decreasing=TRUE)
  u <- matrix(0, ncol=m, nrow=B)

  for(b in seq(B)){
    j <- m
    u[b,j] <- abs(G[b,o[j]])

    while(j > 1){
      j <- j-1
      u[b,j] <- max(u[b,j+1], abs(G[b,o[j]]))
    }
  }

  p <- rep(0,m)
  for(i in seq(m)){
    p[i] <- length(which(u[,i] >= u[1,i]))/B
    if(i > 1){p[i] <- max(p[i], p[i-1])}
  }
  p <- p[order(o, decreasing=FALSE)]
  rej <- which(p < alpha)
  out <- list("pvalues"=p, "rejected"=rej)
  return(out)
}



