#' @title Permutation-Based Multisplit for a Single Variable
#' @description Internal function. It computes the standardized scores for a variable and different flips.
#' @usage jsplitFlip(j, X, Y, sel, fl, exact)
#' @param j index of the variable to be considered.
#' @param X numeric design matrix (excluding the intercept), where columns correspond to variables, and rows to observations.
#' @param Y numeric response vector.
#' @param sel list of observations and variables for different splits, as returned by \code{getSplits}.
#' @param fl numeric matrix, where each column is a sign flip (the first is the identity).
#' @param exact logical, \code{TRUE} for the exact method, \code{FALSE} for the approximate method.
#' @return \code{jsplitFlip} returns a numeric vector containing the standardized scores for the different flips.
#' @author Anna Vesely.
#' @noRd


jsplitFlip <- function(j, X, Y, sel, fl, exact){

  n <- nrow(fl)
  B <- ncol(fl)
  Q <- length(sel$obs)
  out <- rep(0,B) # scores for each flip
  i <- 0 # times that j was selected

  # APPROXIMATE METHOD
  if(!exact){
    R <- matrix(0, ncol=n, nrow=n)

    # for each split, compute and sum the terms R=I-H
    for(q in seq(Q)){
      vars <- sel$vars[[q]]
      obs <- sel$obs[[q]]
      if(!(j %in% vars)){next}

      i <- i+1
      Z <- as.matrix(X[obs, setdiff(vars,j)])
      R[obs, obs] <- R[obs, obs] + residualMatrix(Z)
    }

    if(i == 0){return(out)} # if j was never selected, all scores are zero

    # for each permutation, compute A=RFR and the standardized score
    for(b in seq(B)){
      A <- R %*% diag(fl[,b]) %*% R
      out[b] <- stdScore(X[,j], Y, A)
    }
    return(out)
  }

  # EXACT METHOD
  resMats <- list()

  # for each split, compute and save the terms R=I-H
  for(q in seq(Q)){
    vars <- sel$vars[[q]]
    obs <- sel$obs[[q]]
    if(!(j %in% vars)){next}

    i <- i+1
    R <- matrix(0, ncol=n, nrow=n)
    Z <- as.matrix(X[obs, setdiff(vars,j)])
    R[obs, obs] <- residualMatrix(Z)
    resMats[[i]] <- R
  }

  if(i == 0){return(out)} # if j was never selected, all scores are zero

  # for each permutation, compute A=sum(RFR) and the standardized score
  for(b in seq(B)){
    A <- matrix(0, ncol=n, nrow=n)
    for(i in seq_along(resMats)){A <- A + (resMats[[i]] %*% diag(fl[,b]) %*% resMats[[i]])}
    out[b] <- stdScore(X[,j], Y, A)
  }
  return(out)
}
