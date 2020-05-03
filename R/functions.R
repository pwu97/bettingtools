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
      return(round(US2Implied(american), precisionHelper))
    } else {
      return(NA)
    }
  }

  return(sapply(decimal, convertDecimalToImpliedHelper))
}

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

Implied2Dec <- function(implied, precision = 4) {
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

US2All <- function(american, precision = 4) {
  decimal <- US2Decimal(american)
  implied <- US2Implied(american)
  return(tibble("American" = american,
                "Decimal" = decimal,
                "Implied" = implied))
}

Dec2All <- function(decimal, precision = 4) {
  american <- Dec2US(decimal)
  implied <- Dec2Implied(decimal)
  return(tibble("American" = american,
                "Decimal" = decimal,
                "Implied" = implied))
}

Implied2All <- function(implied, precision = 4) {
  american <- Implied2US(implied)
  decimal <- Implied2Dec(implied)
  return(tibble("American" = american,
                "Decimal" = decimal,
                "Implied" = implied))
}

