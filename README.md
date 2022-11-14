# splitFlip

splitFlip is the package developed to perform resampling-based Multisplit inference for high-dimensional linear regression. It computes test statistics for all variables and random sign-flipping transformations.


## Installation

The latest version of the package can be installed with:

``` r
devtools::install_github("annavesely/splitFlip")
```


## Example

The analysis employs a design matrix (excluding the intercept) and a response vector. Such data may be simulated with the function ```simData```. Here, we are assuming 10 observations of 20 variables, where 2 variables are active.

``` r 
res <- simData(m1=2, m = 20, n = 10, rho = 0.5, type = "toeplitz", SNR = 5, seed = 42)
X <- res$X # design matrix
Y <- res$Y # response vector
active <- res$active # indices of active variables
```

First, we need to choose the technique used to select variables for any random splits of the data. The package contains three functions.

**1.** ```selLasso``` employs the Lasso. If it is possible to estimate the expected number of active variables, we suggest calibrating the lambda parameter to select at most twice the expected number of active variables. 

``` r
target <- 2*length(active)
selLasso(X, Y, target)
```

If no information is available to estimate this number, we suggest using cross-validation.

``` r
selLasso(X, Y)
```

**2.** ```selCorrelation``` selects the first ```target``` variables having the highest correlation (in absolute value) with the response.

``` r
selCorrelation(X, Y, target)
```

**3.** ```selOracle``` selects at most ```target``` variables, always including those in a pre-specified set ```toSel```. This may be useful in simulations to explore the properties of the method when assumptions are met.

``` r
selOracle(X, Y, target, toSel = active)
```

Subsequently, the function ```splitFlip``` employs ```Q``` splits to construct a matrix of permutation test statistics for all variables (columns) and ```B``` random sign-flipping transformations (rows), where the first split is the identity. Other than the variable selection technique, one may choose the type of method: approximate or exact. The latter is generally more powerful but slower, especially when the sample size is high. For instance:

``` r 
# Lasso to select at most target variables
G <- splitFlip(X, Y, target = target, varSel = selLasso, seed = 42)

# Lasso with cross-validation
G <- splitFlip(X, Y, target = NULL, varSel = selLasso, seed = 42)

# oracle selection
G <- splitFlip(X, Y, target, varSel = selLasso, varSelArgs = list(toSel = active), seed = 42)
```

The resulting matrix matrix can be used to obtain p-values and identify variables that are rejected for a given significance level, for instance using the single-step maxT as following.

``` r 
maxT(G, alpha = 0.05)
```



# References
Hemerik, J., Goeman, J. J. and Finos, L. (2020). Robust testing in generalized linear models by sign flipping score contributions. JRSS B, 82(3):841-864.

Meinshausen, N., Meier, L. and Bühlmann, P. (2009). P-values for high-dimensional regression. JASA, 104(488):1671-1681.

Vesely, A., Finos, L. and Goeman, J. J. (2021). Resampling-based Multisplit inference for high-dimensional regression. Pre-print arXiv:2205.12563.



# Did you find some bugs?

Please write to anna.vesely@unipd.it.

