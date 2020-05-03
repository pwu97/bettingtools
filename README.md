<!-- README.md is generated from README.Rmd. Please edit that file -->



# bettingtools

<!-- badges: start -->
<!-- badges: end -->

The bettingtools package has functions related to working with sports betting lines. First, we provide functions to work with American, Decimal, and Implied odds in the tidy format.

## Installation

``` r
remotes::install_github("pwu97/bettingtools")
```

## Calculate zero-vig implied probabilities

We can calculate the zero-vig implied probabilities of a vector of lines. The default precision is set to 4 digits. Note that we can set the precision.


```r
library(bettingtools)

calculateZeroVigProb(c(200, -180, -450, 800))
#> [1] 0.3333 0.6429 0.8182 0.1111

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

## Calculate theoretical hold

We can calculate the theoretical hold for a two-outcome line set. This corresponds to the profit a sportsbook would expect to make were a player to bet on either side of an event with all else being equal. Contrary to popular belief, larger nominal spreads doesn't necessarily mean more profit for bookies.


```r
calculateTheoreticalHold(c(-110, -110))
#> [1] 0.0454

calculateTheoreticalHold(c(-1500, 875))
#> [1] 0.0386

calculateTheoreticalHold(c(-1500, 875), precision = 7)
#> [1] 0.038554
```

## Converting between American, Decimal, and Implied Odds

We can convert American odds to Decimal and Implied Odds.


```r
US2Implied(c(-250, 600, 137, -110))
#> [1] 0.7143 0.1429 0.4219 0.5238

US2Dec(c(-250, 600, 137, -110))
#> [1] 1.40 7.00 2.37 1.91

US2All(c(-250, 600, 137, -110))
#> # A tibble: 4 x 3
#>   American Decimal Implied
#>      <dbl>   <dbl>   <dbl>
#> 1     -250    1.4    0.714
#> 2      600    7      0.143
#> 3      137    2.37   0.422
#> 4     -110    1.91   0.524

Dec2Implied(c(3.17, 2.14, 2.01, 1.67))
#> [1] 0.3155 0.4673 0.4975 0.5988

Dec2US(c(3.17, 2.14, 2.01, 1.67))
#> [1]  217.0000  114.0000  101.0000 -149.2537

Dec2All(c(3.17, 2.14, 2.01, 1.67))
#> # A tibble: 4 x 3
#>   American Decimal Implied
#>      <dbl>   <dbl>   <dbl>
#> 1     217     3.17   0.316
#> 2     114     2.14   0.467
#> 3     101     2.01   0.498
#> 4    -149.    1.67   0.599

Implied2Dec(c(.34, .54, .88, .12))
#> [1] 2.94 1.85 1.14 8.33

Implied2US(c(.34, .54, .88, .12))
#> [1]  194.1176 -117.3913 -733.3333  733.3333

Implied2All(c(.34, .54, .88, .12))
#> # A tibble: 4 x 3
#>   American Decimal Implied
#>      <dbl>   <dbl>   <dbl>
#> 1     194.    2.94    0.34
#> 2    -117.    1.85    0.54
#> 3    -733.    1.14    0.88
#> 4     733.    8.33    0.12
```

