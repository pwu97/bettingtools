% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betting_tools.R
\name{calculateWinRanges}
\alias{calculateWinRanges}
\title{Calculate probabilities of all win possibilities}
\usage{
calculateWinRanges(probabilities)
}
\arguments{
\item{probabilites}{A vector representing a vector of win probabilities.}
}
\value{
A tibble where each row represents one possible win-loss combination
along with the probability of that combination occurring.
}
\description{
Returns a tibble of all possible win-loss combinations.
}
\examples{
calculateWinRanges(c(.1, .4, .88, .47))

calculateWinRanges(c(.12, .462, .29))

calculateWinRanges(c(.6, .6, .6, .6))
}
