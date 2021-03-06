#' @title Data Splits and Variable Selectiong
#' @description Internal function. It splits the data into two subsets of equal size,
#' then uses the first subset to select variables and estimate coefficients.
#' @usage getSplits(X, Y, Q, target, varSel, varSelArgs, seed)
#' @param X numeric design matrix (excluding the intercept), where columns correspond to variables, and rows to observations.
#' @param Y numeric response vector.
#' @param Q numer of data splits.
#' @param target maximum number of variables, i.e. columns of \code{X}, to be selected.
#' @param varSel a function to perform variable selection. It must have at least three arguments:
#' \code{X} (design matrix), \code{Y} (response vector) and \code{target} (maximum number of selected variables).
#' Additional arguments are passed through \code{varSelArgs}.
#' Return value is a numeric vector containing the indices of the selected variables.
#' @param varSelArgs named list of further arguments for \code{varSel}.
#' @param seed seed.
#' @return \code{getSplits} returns a list containing
#' \itemize{
#' \item \code{obs}: list of the \code{Q} index vectors corresponding to observations not used for variable selection
#' \item \code{vars}: list of the \code{Q} index vectors corresponding to selected variables
#' \item \code{signs}: vector containing, for each variable, the mode of the estimated coefficients' signs
#' }
#' @author Anna Vesely.
#' @noRd


getSplits <- function(X, Y, Q, target, varSel, varSelArgs, seed){

  if(is.null(seed)){seed <- sample(seq(10^9), 1)}
  set.seed(round(seed))

  m <- ncol(X)
  n <- nrow(X)
  n0 <- floor(n/2)

  obs <- vector(mode="list", length=Q)
  vars <- vector(mode="list", length=Q)
  signs <- matrix(0, ncol=m, nrow=Q)
  # signs <- rep(0,m) # mode of the estimated coefficients

  for(q in seq(Q)){
    # randomly split the data in 2 subsets
    D1 <- sample(seq(n), n0) # first subset
    V <- sort(do.call(varSel, args=c(list(X=X[D1,], Y=Y[D1], target=target), varSelArgs))) # selected variables
    coeff <- lm(Y[D1] ~ X[D1,V])$coefficients[-1] # coefficients of selected vars

    obs[[q]] <- setdiff(seq(n), D1)
    vars[[q]] <- V
    signs[q,V] <- 2 * (sign(coeff) >= 0) - 1
    # signs[V] <- signs[V] + sign(coeff)
  }

  # signs <- 2*(signs >= 0) - 1 # mode of coefficient's signs

  out <- list("obs"=obs, "vars"=vars, "signs"=signs)
  return(out)
}
