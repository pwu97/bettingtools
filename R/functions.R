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

calculateTheoreticalHold <- function(pair, precision = 4) {
  prob1 <- calculateZeroVigProb(pair[1])
  prob2 <- calculateZeroVigProb(pair[2])
  return(round(1 - 1/(prob1+prob2), precision))
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

convertAmericanToDecimal <- function(american, precision = 4) {
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

