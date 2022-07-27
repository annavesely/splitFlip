#' @title Permutation-Based Multisplit for a Single Variable (new)
#' @description Internal function. It computes the standardized scores for a variable and different flips.
#' @usage jsplitFlip_new(j, X, Y, sel, fl, exact)
#' @param j index of the variable to be considered.
#' @param X numeric design matrix (including the intercept), where columns correspond to variables, and rows to observations.
#' @param Y numeric response vector.
#' @param sel list of observations and variables for different splits, as returned by \code{getSplits}.
#' @param fl numeric matrix, where each column is a sign flip (the first is the identity).
#' @param exact logical, \code{TRUE} for the exact method, \code{FALSE} for the approximate method.
#' @return \code{jsplitFlip_new} returns a numeric vector containing the standardized scores for the different flips.
#' @author Anna Vesely.
#' @noRd


jsplitFlip_new <- function(j, X, Y, sel, fl, exact){

  n <- nrow(fl)
  B <- ncol(fl)
  Q <- length(sel$obs)
  out <- rep(0,B) # scores for each flip
  i <- 0 # times that j was selected

  qsel <- which(sel$signs[,j] != 0) # indices of splits that select j
  jsigns <- sel$signs[qsel,j] # signs for the splits that select j
  K <- length(qsel)

  if(K == 0){return(out)} # if j was never selected, all scores are zero


  # APPROXIMATE METHOD (TO FIX)
  if(!exact){
    R <- matrix(0, ncol=n, nrow=n)

    # for each split, compute and sum the terms R=I-H
    k <- 0

    for(q in qsel){
      k <- k+1
      R <- R + (jsigns[k] * residualMatrix(j, X, sel$obs[[q]], setdiff(sel$vars[[q]],j)))
    }

    # for each permutation, compute A=RFR and the standardized score
    for(b in seq(B)){
      A <- R %*% diag(fl[,b]) %*% R
      out[b] <- stdScore(X[,j], Y, A)
    }
    return(out)
  }



  # EXACT METHOD
  resMats <- vector(mode="list", length=K)

  # for each split, compute and save the terms R=I-H
  k <- 0

  for(q in qsel){
    k <- k+1
    resMats[[k]] <- residualMatrix(j, X, sel$obs[[q]], setdiff(sel$vars[[q]],j))
  }

  # for each permutation, compute A=sum(RFR) and the standardized score
  for(b in seq(B)){
    A <- matrix(0, ncol=n, nrow=n)
    for(k in seq(K)){
      A <- A + (jsigns[k] * resMats[[k]] %*% diag(fl[,b]) %*% resMats[[k]])
    }
    out[b] <- stdScore(X[,j], Y, A)
  }
  return(out )
}
