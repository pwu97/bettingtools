library(tibble)
library(dplyr)

#' @title Convert American to Decimal odds
#'
#' @description Returns a vector of Decimal odds
#'
#' @param american A vector of American odds.
#' @param precision A numerical value representing the precision. The default precision
#' is set to 4 digits.
#'
#' @return A vector of Decimal odds.
#'
#' @examples
#' US2Dec(c(-250, 600, 137, -110))
US2Dec <- function(american, precision = 2) {
  US2DecHelper <- function(one_american, precisionHelper = precision) {
    if (one_american >= 100) {
      return(round(one_american/100 + 1, precisionHelper))
    } else if (one_american <= -100) {
      return(round(100/(-1.0 * one_american) + 1, precisionHelper))
    } else {
      return(NA)
    }
  }

  return(sapply(american, US2DecHelper))
}

#' @title Convert American to Implied odds
#'
#' @description Returns a vector of Implied odds
#'
#' @param american A vector of American odds.
#' @param precision A numerical value representing the precision. The default precision
#' is set to 4 digits.
#'
#' @return A vector of Implied odds.
#'
#' @examples
#' US2Implied(c(-250, 600, 137, -110))
US2Implied <- function(american, precision = 4) {
  US2ImpliedHelper <- function(one_american, precisionHelper = precision) {
    if (one_american >= 100) {
      return(round(100/(one_american + 100), precisionHelper))
    } else if (one_american <= -100) {
      return(round((-1.0 * one_american)/(-1.0 * one_american + 100), precisionHelper))
    } else {
      return(NA)
    }
  }

  return(sapply(american, US2ImpliedHelper))
}

#' @title Calculate fair lines from a pair of American odds
#'
#' @description Returns a vector of fair lines
#'
#' @param pair A vector representing the two outcomes in American odds format.
#' @param precision A numerical value representing the precision. The default precision
#' is set to 4 digits.
#'
#' @return A vector representing the fair line
#'
#' @examples
#' US2Fair(c(265, -375))
#'
#' US2Fair(c(150, -200))
US2Fair <- function(pair, precision = 4) {
  prob1 <- calculateImpliedProbPair(pair, precision)[1]
  prob2 <- calculateImpliedProbPair(pair, precision)[2]

  return(c(Implied2US(prob1, precision), Implied2US(prob2, precision)))
}

#' @title Convert Decimal to American odds
#'
#' @description Returns a vector of American odds
#'
#' @param decimal A vector of Decimal odds.
#' @param precision A numerical value representing the precision. The default precision
#' is set to 4 digits.
#'
#' @return A vector of American odds.
#'
#' @examples
#' Dec2US(c(3.17, 2.14, 2.01, 1.67))
Dec2US <- function(decimal, precision = 4) {
  Dec2USHelper <- function(one_decimal, precisionHelper = precision) {
    if (one_decimal >= 2) {
      return(round((one_decimal - 1) * 100, precisionHelper))
    } else if ((one_decimal < 2) & (one_decimal >= 1)) {
      return(round(-100/(one_decimal - 1), precisionHelper))
    } else {
      return(NA)
    }
  }

  return(sapply(decimal, Dec2USHelper))
}

#' @title Convert Decimal to Implied odds
#'
#' @description Returns a vector of Implied odds
#'
#' @param decimal A vector of Decimal odds.
#' @param precision A numerical value representing the precision. The default precision
#' is set to 4 digits.
#'
#' @return A vector of Implied odds.
#'
#' @examples
#' Dec2Implied(c(3.17, 2.14, 2.01, 1.67))
Dec2Implied <- function(decimal, precision = 4) {
  Dec2ImpliedHelper <- function(one_decimal, precisionHelper = precision) {
    if ((one_decimal >= 2) |  (one_decimal < 2) & (one_decimal > 1)) {
      american <- Dec2US(one_decimal)
      return(round(US2Implied(american), precisionHelper))
    } else {
      return(NA)
    }
  }

  return(sapply(decimal, Dec2ImpliedHelper))
}

#' @title Calculate fair lines from a pair of Decimal odds
#'
#' @description Returns a vector of fair lines
#'
#' @param pair A vector representing the two outcomes in Decimal odds format.
#' @param precision A numerical value representing the precision. The default precision
#' is set to 4 digits.
#'
#' @return A vector representing the fair line
#'
#' @examples
#' Dec2Fair(c(2.14, 1.86))
#'
#' Dec2Fair(c(4.00, 1.51))
Dec2Fair <- function(pair, precision = 4) {
  pair_in_us <- Dec2US(pair)
  prob1 <- calculateImpliedProbPair(pair_in_us, precision)[1]
  prob2 <- calculateImpliedProbPair(pair_in_us, precision)[2]

  return(c(Implied2US(prob1, precision), Implied2US(prob2, precision)))
}

#' @title Convert Implied to American odds
#'
#' @description Returns a vector of American odds
#'
#' @param decimal A vector of Implied odds.
#' @param precision A numerical value representing the precision. The default precision
#' is set to 4 digits.
#'
#' @return A vector of American odds.
#'
#' @examples
#' Implied2US(c(.34, .54, .88, .12))
Implied2US <- function(implied, precision = 4) {
  Implied2USHelper <- function(one_implied, precisionHelper = precision) {
    if ((one_implied >= 0.5) & (one_implied <= 1)) {
      return(round((100 * one_implied)/(-1 + one_implied), precisionHelper))
    } else if ((one_implied >= 0) & (one_implied < 0.5)) {
      return(round((100 - 100 * one_implied)/one_implied, precisionHelper))
    } else {
      return(NA)
    }
  }

  return(sapply(implied, Implied2USHelper))
}

#' @title Convert Implied to Decimal odds
#'
#' @description Returns a vector of Decimal odds
#'
#' @param decimal A vector of Implied odds.
#' @param precision A numerical value representing the precision. The default precision
#' is set to 4 digits.
#'
#' @return A vector of Decimal odds.
#'
#' @examples
#' Implied2Dec(c(.34, .54, .88, .12))
Implied2Dec <- function(implied, precision = 2) {
  Implied2DecHelper <- function(one_implied, precisionHelper = precision) {
    if ((one_implied >= 0) & (one_implied <= 1)) {
      american <- Implied2US(one_implied)
      return(round(US2Dec(american), precisionHelper))
    } else {
      return(NA)
    }
  }

  return(sapply(implied, Implied2DecHelper))
}

#' @title Convert American to Decimal and Implied odds
#'
#' @description Returns a tibble where each row has American, Decimal, and Implied
#' odds
#'
#' @param american A vector of American odds.
#' @param precision A numerical value representing the precision. The default precision
#' is set to 4 digits.
#'
#' @return A tibble of American, Decimal, and Implied odds
#'
#' @examples
#' US2All(c(-250, 600, 137, -110))
US2All <- function(american, precision = 4) {
  decimal <- US2Dec(american, precision)
  implied <- US2Implied(american, precision)
  return(tibble("American" = american,
                "Decimal" = decimal,
                "Implied" = implied))
}

#' @title Convert Decimal to American and Implied odds
#'
#' @description Returns a tibble where each row has American, Decimal, and Implied
#' odds
#'
#' @param american A vector of Decimal odds.
#' @param precision A numerical value representing the precision. The default precision
#' is set to 4 digits.
#'
#' @return A tibble of American, Decimal, and Implied odds
#'
#' @examples
#' Dec2All(c(3.17, 2.14, 2.01, 1.67))
Dec2All <- function(decimal, precision = 4) {
  american <- Dec2US(decimal, precision)
  implied <- Dec2Implied(decimal, precision)
  return(tibble("American" = american,
                "Decimal" = decimal,
                "Implied" = implied))
}

#' @title Convert Implied to Decimal and Implied odds
#'
#' @description Returns a tibble where each row has American, Decimal, and Implied
#' odds
#'
#' @param implied A vector of Implied odds.
#' @param precision A numerical value representing the precision. The default precision
#' is set to 4 digits.
#'
#' @return A tibble of American, Decimal, and Implied odds
#'
#' @examples
#' Implied2All(c(.34, .54, .88, .12))
Implied2All <- function(implied, precision = 4) {
  american <- Implied2US(implied, precision)
  decimal <- Implied2Dec(implied, precision)
  return(tibble("American" = american,
                "Decimal" = decimal,
                "Implied" = implied))
}

