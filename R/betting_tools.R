library(tibble)
library(dplyr)

#' @title Calculate probabilities of all win possibilities
#'
#' @description Returns a tibble of all possible win-loss combinations.
#'
#' @param probabilites A vector representing a vector of win probabilities.
#'
#' @return A tibble where each row represents one possible win-loss combination
#' along with the probability of that combination occurring.
#'
#' @examples
#' calculateWinRanges(c(.1, .4, .88, .47))
#'
#' calculateWinRanges(c(.12, .462, .29))
#'
#' calculateWinRanges(c(.6, .6, .6, .6))
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

#' @title Calculates single Kelly stakes
#'
#' @description Returns a vector of single Kelly stake. This is the percentage of one's
#' bankroll one should bet to maximize the expected growth of one's bankroll on a single bet.
#' This is known as the single Kelly stake.
#'
#' @param expected A numerical value representing the odds of the expected payout.
#' @param payout A numerical value representing the odds of the actual payout.
#' @param kelly_multiplier A numerical value representing the kelly multiplier of the bet.
#' The default value of the Kelly multiplier is 1.
#' @param expected_odds A string representing the odds format of the expected payout.
#' It is either "prob", "dec", or "us". The default odds format is in implied probability.
#' @param payout_odds A string representing the odds format of the actual payout.
#' It is either "prob", "dec", or "us". The default odds format is in decimal odds.
#'
#' @return The vector of single Kelly stakes.
#'
#' @examples
#' calculateKellyStake(0.41, 2.56)
#'
#' calculateKellyStake(-120, 150, expected_odds = "us", payout_odds = "us")
#'
#' calculateKellyStake(0.70, -150, kelly_multiplier = 0.1, payout_odds = "us")
#'
#' calculateKellyStake(0.26, -110, payout_odds = "us")
#'
#' calculateKellyStake(c(0.41, 0.89, 0.24), c(-103, 780, 400), payout_odds = "us")
calculateKellyStake <- function(expected, payout, kelly_multiplier = 1,
                                expected_odds = "prob",
                                payout_odds = "dec",
                                precision = 4) {
  # Calculate a single Kelly stake
  calculateKellyStakeHelper <- function(expected_helper, payout_helper) {
    odds <- rep(NA, 2)
    if (expected_odds == "prob") {
      odds[1] <- expected_helper
    } else if (expected_odds == "dec") {
      odds[1] <- Dec2Implied(expected_helper)
    } else if (expected_odds == "us") {
      odds[1] <- US2Implied(expected_helper)
    }

    if (payout_odds == "dec") {
      odds[2] <- payout_helper
    } else if (payout_odds == "prob") {
      odds[2] <- Implied2Dec(payout_helper)
    } else if (payout_odds == "us") {
      odds[2] <- US2Dec(payout_helper)
    }

    return(round(max(kelly_multiplier *
                     ((odds[1] * odds[2] - 1)/(odds[2] - 1)), 0), precision))
  }

  # Prepare the vector to return
  result <- c()
  for (i in 1:length(expected)) {
    result <- c(result, calculateKellyStakeHelper(expected[i], payout[i]))
  }
  return(result)
}

#' @title Calculates the theoretical hold
#'
#' @description Returns the theoretical hold for a two-outcome line set. This corresponds
#' to the profit a sportsbook would expect to make were a player to bet on either side
#' of an event with all else being equal.
#'
#' @param pair A vector representing the pair of outcomes in American odds format
#' @param precision A numerical value representing the precision. The default precision
#' is set to 4 digits.
#'
#' @return A numerical value representing the theoretical hold.
#'
#' @examples
#' calculateTheoreticalHold(c(-110, -110))
#'
#' calculateTheoreticalHold(c(-1500, 875))
#'
#' calculateTheoreticalHold(c(-1500, 875), precision = 7)
calculateTheoreticalHold <- function(pair, precision = 4) {
  prob1 <- calculateZeroVigProb(pair[1])
  prob2 <- calculateZeroVigProb(pair[2])
  return(round(1 - 1/(prob1 + prob2), precision))
}

#' @title Calculate zero-vig implied probabilities
#'
#' @description Returns a vector of the zero-vig implied probabilities
#'
#' @param lines A vector representing a vector of lines in American odds format.
#' @param precision A numerical value representing the precision. The default precision
#' is set to 4 digits.
#'
#' @return A vector representing the zero-vig implied probabilities.
#'
#' @examples
#' calculateZeroVigProb(c(200, -180, -450, 800))
#'
#' calculateZeroVigProb(-237)
#'
#' calculateZeroVigProb(-237, precision = 7)
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

#' @title Calculate implied probabilities
#'
#' @description Returns a vector of implied probabilities
#'
#' @param pair A vector representing the several (n >= 2) outcomes in American odds format.
#' @param precision A numerical value representing the precision. The default precision
#' is set to 4 digits.
#'
#' @return A vector representing the implied probabilites
#'
#' @examples
#' calculateNormalizedImplied(c(1000, -800), precision = 7)
#'
#' calculateNormalizedImplied(c(427, -213, 336))
calculateNormalizedImplied <- function(input, precision = 4) {
  probs <- rep(NA, length(input))
  for (i in 1:length(input)) {
    probs[i] <- calculateZeroVigProb(input[i], precision = precision)
  }
  for (i in 1:length(input)) {
    probs[i] <- round(probs[i]/sum(probs), precision)
  }

  return(probs)
}

