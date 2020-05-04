<!-- README.md is generated from README.Rmd. Please edit that file -->



# bettingtools

<!-- badges: start -->
<!-- badges: end -->

The bettingtools package has functions related to working with sports betting lines. First, we provide functions to work with American, Decimal, and Implied odds in the tidy format.

## Installation

``` r
remotes::install_github("pwu97/bettingtools")
```

## Calculate single Kelly stake

We can calculate the percentage of one's bankroll one should bet to maximize the expected growth of one's bankroll on a single bet. Given an expected win probability, payout odds, and an optional Kelly multiplier factor, we can calculate one's optimal single Kelly stake. Note that default odds for the expected win probability is implied probability and the default odds for the payout is in decimal. We can change them accordingly to how we see fit by specifying additional parameters in our function.


```r
library(bettingtools)

calculateKellyStake(0.53, 1.92)
#> [1] 0.0191

calculateKellyStake(0.41, 2.56)
#> [1] 0.0318

calculateKellyStake(0.70, -150, kelly_multiplier = 0.1, payout_odds = "us")
#> [1] 0.0252

calculateKellyStake(0.26, 367, payout_odds = "us")
#> [1] 0.0584

# Optimal move is to not place a bet
calculateKellyStake(0.26, -110, payout_odds = "us")
#> [1] 0
```


## Calculate zero-vig implied probabilities

We can calculate the zero-vig implied probabilities of a vector of lines. The default precision is set to 4 digits. Note that we can set the precision.


```r
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
#> Error in tibble(American = american, Decimal = decimal, Implied = implied): could not find function "tibble"

Dec2Implied(c(3.17, 2.14, 2.01, 1.67))
#> [1] 0.3155 0.4673 0.4975 0.5988

Dec2US(c(3.17, 2.14, 2.01, 1.67))
#> [1]  217.0000  114.0000  101.0000 -149.2537

Dec2All(c(3.17, 2.14, 2.01, 1.67))
#> Error in tibble(American = american, Decimal = decimal, Implied = implied): could not find function "tibble"

Implied2Dec(c(.34, .54, .88, .12))
#> [1] 2.94 1.85 1.14 8.33

Implied2US(c(.34, .54, .88, .12))
#> [1]  194.1176 -117.3913 -733.3333  733.3333

Implied2All(c(.34, .54, .88, .12))
#> Error in tibble(American = american, Decimal = decimal, Implied = implied): could not find function "tibble"
```

