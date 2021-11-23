# splitFlip

splitFlip is the package developed to compute permutation standardized scores for high-dimensional linear regression.


## Installation

The latest version of the package can be installed with:

``` r
devtools::install_github("annavesely/splitFlip")
```


## Example

The analysis employs a design matrix (excluding the intercept) and a response vector. Such data may be simulated with the function ```simData```. Here, we are assuming 20 variables and 10 observations, where 10% of the variables are active.

``` r 
res <- simData(prop=0.1, m=20, n=10, seed=42)
```

TO FINISH (also check documentation of simData)

# Did you find some bugs?

Please write to anna.vesely@phd.unipd.it.

