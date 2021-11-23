#' @title Standardized Score
#' @description Internal function. It computes the standardized scores for a variable and a flip.
#' @usage stdScore(v, Y, A)
#' @param v numeric vector of observations for a covariate.
#' @param Y numeric response vector.
#' @param A numeric matrix, as defined within the function \code{jsplitFlip}.
#' @details The standardized score is the ratio of the effective score \code{t(v)AY} and its variance.
#' @details The matrix \code{A} is defined as (or approximates) the sum of the terms \code{R(q)FR(q)} (\code{q=1,...,Q}),
#' where \code{R(q)} is the residual matrix for the \code{q}-th split, and \code{F} is a sign flip.
#' Hence the effective score \code{t(v)AY} is equal to (or approximates) the sum of the
#' effective scores computed for the different splits.
#' @return \code{stdScore} returns a numeric value for the standardized score.
#' @author Anna Vesely.
#' @noRd


stdScore <- function(v, Y, A){
  effective <- t(v) %*% A %*% Y
  variance <- sum((t(v) %*% A)^2)
  out <- effective / sqrt(variance)
  return(out)
}
