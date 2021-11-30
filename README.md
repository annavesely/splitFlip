# splitFlip

splitFlip is the package developed to compute permutation standardized scores for high-dimensional linear regression.


## Installation

The latest version of the package can be installed with:

``` r
devtools::install_github("annavesely/splitFlip")
```


## Example

The analysis employs a design matrix (excluding the intercept) and a response vector. Such data may be simulated with the function ```simData```. Here, we are assuming 80 observations of 100 variables, where 5 variables are active.

``` r 
res <- simData(prop = 0.05, m = 100, n = 100, rho = 0.5, type = "toeplitz", SNR = 1, seed = 42)
X <- res$X # design matrix
Y <- res$Y # response vector
active <- res$active # indices of active variables
```

First, we need to set up how many variables should be selected in each split. A reasonable approach is to estimate the number of active variables that we expect to have, and multiply it by two (notice that in this example we know this number).

``` r 
target <- 2 * length(active)
```

Subsequently, we need to choose the function that will be used for selection. This may be any function whose arguments are (at least) ```X```, ```Y``` and ```target```, and which returns the indices of selected variables. The package contains two functions.

**1.** The function ```targetLasso``` employs the Lasso, calibrating its parameter to select at most ```target``` variables.

``` r
targetLasso(X, Y, target)
```

**2.** The function ```targetOracle``` selects at most ```target``` variables, always including those in a pre-specified set ```toSel```.

``` r
targetOracle(X, Y, target, toSel = active)
```

Finally, the function ```splitFlip``` employs ```Q``` splits to construct a matrix of standardized scores for all variables (columns) and random sign flips (rows), where the first split is the identity. Other than the selection settings, one may choose the type of method: approximate or exact. The latter is generally more powerful but slower, especially when the sample size is high. In this setting, for instance, the computation time is around 4s for the approximate, and 12s for the exact.

``` r 
G <- splitFlip(X, Y, Q = 10, exact = FALSE, target = target, varSel = targetLasso, seed = 42)
```

This matrix can be used to obtain p-values and identify variables that are rejected for a given significance level, for instance using the single-step maxT as following.

``` r 
maxT(G, alpha = 0.05)
```



# Did you find some bugs?

Please write to anna.vesely@phd.unipd.it.

