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

We can calculate the zero-vig implied probabilities of a vector of lines. The default precision is set to 4 digits. Note that we can set the precision.


```r
library(bettingtools)
#> 
#> Attaching package: 'bettingtools'
#> The following objects are masked _by_ '.GlobalEnv':
#> 
#>     calculateImpliedProbPair, calculateTheoreticalHold, calculateZeroVigProb,
#>     convertAmericanToDecimal

calculateZeroVigProb(c(200, -180, -450, 700))
#> [1] 0.3333 0.6429 0.8182 0.1250

calculateZeroVigProb(-237)
#> [1] 0.7033

calculateZeroVigProb(-237, precision = 7)
#> [1] 0.7032641
```

##  Calculate implied probabilities for two-outcome line set

We can calculate the implied probabilities for two lines by first calculating the zero-vig implied probabilities for both of them, and then normalizing them. Again, we can set the precision. 


```r
calculateImpliedProbPair(c(200, -220))
#> [1] 0.3265 0.6735

calculateImpliedProbPair(c(1000, -800))
#> [1] 0.0928 0.9072

calculateImpliedProbPair(c(1000, -800), precision = 7)
#> [1] 0.0927835 0.9072165
```

