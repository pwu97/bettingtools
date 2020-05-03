# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(tibble)

calculateTheoreticalHold <- function(pair, precision = 4) {
  prob1 <- calculateZeroVigProb(pair[1])
  prob2 <- calculateZeroVigProb(pair[2])
  return(round(1 - 1/(prob1 + prob2), precision))
}

calculateZeroVigProb <- function(lines, precision = 4) {
  calculateZeroVigProbHelper <- function(line, precisionHelper = precision) {
    if (line >= 100) {
      return(round(100/(100+line), precisionHelper))
    } else if (line <= -100) {
      return(round((-1.0*line)/(100-line), precisionHelper))
    } else {
      return(NA)
    }
  }

  return(sapply(lines, calculateZeroVigProbHelper))
}

calculateImpliedProbPair <- function(pair, precision = 4) {
  prob1 <- calculateZeroVigProb(pair[1], precision = precision)
  prob2 <- calculateZeroVigProb(pair[2], precision = precision)
  return(c(round(prob1/(prob1+prob2), precision), round(prob2/(prob1+prob2), precision)))
}

US2Dec <- function(american, precision = 2) {
  convertAmericanToDecimalHelper <- function(one_american, precisionHelper = precision) {
    if (one_american >= 100) {
      return(round(one_american/100 + 1, precisionHelper))
    } else if (one_american <= -100) {
      return(round(100/(-1.0 * one_american) + 1, precisionHelper))
    } else {
      return(NA)
    }
  }

  return(sapply(american, convertAmericanToDecimalHelper))
}

US2Implied <- function(american, precision = 4) {
  convertAmericanToDecimalHelper <- function(one_american, precisionHelper = precision) {
    if (one_american >= 100) {
      return(round(100/(one_american + 100), precisionHelper))
    } else if (one_american <= -100) {
      return(round((-1.0 * one_american)/(-1.0 * one_american + 100), precisionHelper))
    } else {
      return(NA)
    }
  }

  return(sapply(american, convertAmericanToDecimalHelper))
}

Dec2US <- function(decimal, precision = 4) {
  convertDecimalToAmericanHelper <- function(one_decimal, precisionHelper = precision) {
    if (one_decimal >= 2) {
      return(round((one_decimal - 1) * 100, precisionHelper))
    } else if ((one_decimal < 2) & (one_decimal >= 1)) {
      return(round(-100/(one_decimal - 1), precisionHelper))
    } else {
      return(NA)
    }
  }

  return(sapply(decimal, convertDecimalToAmericanHelper))
}

Dec2Implied <- function(decimal, precision = 4) {
  convertDecimalToImpliedHelper <- function(one_decimal, precisionHelper = precision) {
    if ((one_decimal >= 2) |  (one_decimal < 2) & (one_decimal > 1)) {
      american <- convertDecimalToAmerican(one_decimal)
      return(round(convertAmericanToImplied(american), precisionHelper))
    } else {
      return(NA)
    }
  }

  return(sapply(decimal, convertDecimalToImpliedHelper))
}

Implied2US <- function(implied, precision = 4) {
  convertImpliedToAmericanHelper <- function(one_implied, precisionHelper = precision) {
    if ((one_implied >= 0.5) & (one_implied <= 1)) {
      return(round((100 * one_implied)/(-1 + one_implied), precisionHelper))
    } else if ((one_implied >= 0) & (one_implied < 0.5)) {
      return(round((100 - 100 * one_implied)/one_implied, precisionHelper))
    } else {
      return(NA)
    }
  }

  return(sapply(implied, convertImpliedToAmericanHelper))
}

Implied2US <- function(implied, precision = 4) {
  convertImpliedToDecimalHelper <- function(one_implied, precisionHelper = precision) {
    if ((one_implied >= 0) & (one_implied <= 1)) {
      american <- convertImpliedToAmerican(one_implied)
      return(round(convertAmericanToDecimal(american), precisionHelper))
    } else {
      return(NA)
    }
  }

  return(sapply(implied, convertImpliedToDecimalHelper))
}

US2All <- function(american, precision = 4) {
  decimal <- convertAmericanToDecimal(american)
  implied <- convertAmericanToImplied(american)
  return(tibble("American" = american,
                "Decimal" = decimal,
                "Implied" = implied))
}

