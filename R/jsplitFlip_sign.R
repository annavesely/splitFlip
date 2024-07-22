#' @title Permutation-Based Multisplit for a Single Variable (signed)
#' @description Internal function. It computes the standardized scores for a variable and different flips.
#' @usage jsplitFlip_sign(j, X, Y, sel, fl, exact)
#' @param j index of the variable to be considered.
#' @param X numeric design matrix (including the intercept), where columns correspond to variables, and rows to observations.
#' @param Y numeric response vector.
#' @param sel list of observations and variables for different splits, as returned by \code{getSplits}.
#' @param fl numeric matrix, where each column is a sign flip (the first is the identity).
#' @param exact logical, \code{TRUE} for the exact method, \code{FALSE} for the approximate method.
#' @param sign_type type of method; 0 for unsigned, 1 for the mode of the coefficient's signs, 2 for the individual signs.
#' @return \code{jsplitFlip_sign} returns a numeric vector containing the standardized scores for the different flips.
#' @author Anna Vesely.
#' @noRd


jsplitFlip_sign <- function(j, X, Y, sel, fl, exact, sign_type){

  n <- nrow(fl)
  B <- ncol(fl)
  Q <- length(sel$obs)
  out <- rep(0,B) # scores for each flip
  k <- 0 # selections of the variable

  # RIPRENDERE USANDO LA MATRICE DEI COEFFICIENTI E CONSIDERANDO ANCHE "unsigned"

  qsel <- which(sel$signs[,j] != 0) # indices of splits that select j
  K <- length(qsel)

  if(K == 0){return(out)} # if j was never selected, all scores are zero

  jsigns <- sel$signs[qsel,j] # signs for the splits that select j
  jmode <- 2*(sum(jsigns) >= 0) - 1




  # APPROXIMATE METHOD

  if(!exact){
    R <- matrix(0, ncol=n, nrow=n)

    # for each split, compute and sum the terms R=I-H
    for(q in qsel){
      k <- k+1
      R <- R + (residualMatrix(j, X, sel$obs[[q]], setdiff(sel$vars[[q]],j)))
    }


    # for each permutation, compute A=RFR and the standardized score
    for(b in seq(B)){
      A <- R %*% diag(fl[,b]) %*% R
      out[b] <- stdScore(X[,j], Y, A)
    }

    if(sign_type == 1){
      out <- out * jmode #sel$sign[j]
    }

    return(out)
  }






  # EXACT METHOD
  resMats <- vector(mode="list", length=K)

  # for each split, compute and save the terms R=I-H

  for(q in qsel){
    k <- k+1
    resMats[[k]] <- residualMatrix(j, X, sel$obs[[q]], setdiff(sel$vars[[q]],j))
  }



  # for each permutation, compute A=sum(RFR) and the standardized score
  for(b in seq(B)){
    A <- matrix(0, ncol=n, nrow=n)

    for(k in seq(K)){

      tmp <- resMats[[k]] %*% diag(fl[,b]) %*% resMats[[k]]

      if(sign_type == 2){
        A <- A + (jsigns[k] * tmp)
      }else{
        A <- A + tmp
      }
    }

    out[b] <- stdScore(X[,j], Y, A)
  }

  if(sign_type == 1){
    out <- out * jmode #sel$sign[j]
  }

  return(out)
}
