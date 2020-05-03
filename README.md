<!-- README.md is generated from README.Rmd. Please edit that file -->



# bettingtools

<!-- badges: start -->
<!-- badges: end -->

The bettingtools package has functions related to working with sports betting lines.

## Installation

``` r
remotes::install_github("pwu97/bettingtools")
```

## Calculate zero-vig implied probabilities

We can calculate the zero-vig implied probabilities of a vector of lines.


```r
library(bettingtools)

calculateZeroVigProb(c(200, -180, -450, 700))
#> [1] 0.3333 0.6429 0.8182 0.1250

calculateZeroVigProb(c(-250))
#> [1] 0.7143
```

##  Calculate implied probabilities for two-outcome line set


```r
calculateZeroVigProbPair(c(200, -220))
#> [1] 0.327 0.673

calculateZeroVigProbPair(c(1000, -800))
#> [1] 0.093 0.907
```

