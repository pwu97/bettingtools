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

calculateWinRanges <- function(probabilities) {
  win_probs <- rep(NA, length(probabilities) + 1)

  # W = 0
  win_probs[1] <- prod(1 - probabilities)
  # W = 1 to length(probabilities)
  for (i in 1:length(probabilities)) {
    mat <- combn(probabilities, i)
    cur_prob <- 0
    for (j in 1:ncol(mat)) {
      win_vectors <- mat[, j]
      loss_vectors <- win_probs[1]/prod(1 - win_vectors)
      cur_prob <- cur_prob + prod(win_vectors) * prod(loss_vectors)
    }
    win_probs[i+1] <- cur_prob
  }

  # Create return tibble
  return(tibble("W" = c(0:length(probabilities)),
                "L" = rev(c(0:length(probabilities))),
                "Probability" = win_probs))
}

calculateKellyStake <- function(expected, payout, kelly_multiplier = 1,
                                expected_odds = "prob",
                                payout_odds = "dec") {
  odds <- rep(NA, 2)
  if (expected_odds == "prob") {
    odds[1] <- expected
  } else if (expected_odds == "dec") {
    odds[1] <- Dec2Implied(expected)
  } else if (expected_odds == "us") {
    odds[1] <- US2Implied(expected)
  }

  if (payout_odds == "dec") {
    odds[2] <- payout
  } else if (payout_odds == "prob") {
    odds[2] <- Implied2Dec(payout)
  } else if (payout_odds == "us") {
    odds[2] <- US2Dec(payout)
  }

  return(round(max(kelly_multiplier * ((odds[1] * odds[2] - 1)/(odds[2] - 1)), 0), 4))
}

calculateTheoreticalHold <- function(pair, precision = 4) {
  prob1 <- calculateZeroVigProb(pair[1])
  prob2 <- calculateZeroVigProb(pair[2])
  return(round(1 - 1/(prob1 + prob2), precision))
}

calculateZeroVigProb <- function(lines, precision = 4) {
  calculateZeroVigProbHelper <- function(line, precisionHelper = precision) {
    if (line >= 100) {
      return(round(100/(100 + line), precisionHelper))
    } else if (line <= -100) {
      return(round((-1.0 * line)/(100 - line), precisionHelper))
    } else {
      return(NA)
    }
  }

  return(sapply(lines, calculateZeroVigProbHelper))
}

calculateImpliedProbPair <- function(pair, precision = 4) {
  prob1 <- calculateZeroVigProb(pair[1], precision = precision)
  prob2 <- calculateZeroVigProb(pair[2], precision = precision)
  return(c(round(prob1/(prob1 + prob2), precision),
           round(prob2/(prob1 + prob2), precision)))
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

US2All <- function(american, precision = 4) {
  decimal <- US2Dec(american, precision)
  implied <- US2Implied(american, precision)
  return(tibble("American" = american,
                "Decimal" = decimal,
                "Implied" = implied))
}

Dec2All <- function(decimal, precision = 4) {
  american <- Dec2US(decimal, precision)
  implied <- Dec2Implied(decimal, precision)
  return(tibble("American" = american,
                "Decimal" = decimal,
                "Implied" = implied))
}

Implied2All <- function(implied, precision = 4) {
  american <- Implied2US(implied, precision)
  decimal <- Implied2Dec(implied, precision)
  return(tibble("American" = american,
                "Decimal" = decimal,
                "Implied" = implied))
}

