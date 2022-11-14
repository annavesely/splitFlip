#' @title Data Splits and Variable Selection
#' @description Internal function. It splits the data into two subsets of equal size,
#' then uses the first subset to select variables.
#' @usage getSplits(X, Y, Q, target, varSel, varSelArgs, maxRepeat, seed)
#' @param X numeric design matrix (excluding the intercept), where columns correspond to variables, and rows to observations.
#' @param Y numeric response vector.
#' @param Q numer of data splits.
#' @param target maximum number of variables to be selected.
#' @param varSel a function to perform variable selection. It must have at least three arguments:
#' \code{X} (design matrix), \code{Y} (response vector), and \code{target} (maximum number of variables to be selected).
#' Additional arguments are passed through \code{varSelArgs}.
#' Return value is a numeric vector containing the indices of the selected variables.
#' @param varSelArgs named list of further arguments for \code{varSel}.
#' @param maxRepeat maximum number of split trials.
#' @param seed seed.
#' @details If too many variables are selected in a split (more than half the sample size),
#' a warning is returned and the data is randomly split again.
#' After \code{maxRepeat} trials where too many variables are selected,
#' the function returns an error message.
#' @return \code{getSplits} returns a list containing
#' \itemize{
#' \item \code{obs}: list of the \code{Q} index vectors corresponding to observations not used for variable selection
#' \item \code{vars}: list of the \code{Q} index vectors corresponding to selected variables
#' }
#' @author Anna Vesely.
#' @noRd

#\item \code{signs}: vector containing, for each variable, the mode of the estimated coefficients' signs

getSplits <- function(X, Y, Q, target, varSel, varSelArgs, maxRepeat, seed){

  if(is.null(seed)){seed <- sample(seq(10^9), 1)}
  set.seed(round(seed))

  m <- ncol(X)
  n <- nrow(X)
  n0 <- floor(n/2)

  obs <- vector(mode="list", length=Q)
  vars <- vector(mode="list", length=Q)
  # signs <- matrix(0, ncol=m, nrow=Q)

  for(q in seq(Q)){
    tryAgain <- TRUE
    h <- 1

    while(tryAgain){
      # randomly split the data in 2 subsets
      D1 <- sample(seq(n), n0) # first subset
      V <- do.call(varSel, args=c(list(X=X[D1,], Y=Y[D1], target=target), varSelArgs)) # selected variables

      if(length(V) <= n0){
        obs[[q]] <- setdiff(seq(n), D1)
        if(length(V) > 0){vars[[q]] <- V}
        tryAgain <- FALSE
      }else{
        h <- h + 1
        warning("Too large model selected in a sample-split")
        if(h > maxRepeat){stop("More than maxRepeat=", maxRepeat, " sample splits resulted in too large models")}
      }
    }
  }
  out <- list("obs"=obs, "vars"=vars)
  return(out)
}
