% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betting_tools.R
\name{calculateImpliedProbPair}
\alias{calculateImpliedProbPair}
\title{Calculate implied probabilities}
\usage{
calculateImpliedProbPair(pair, precision = 4)
}
\arguments{
\item{pair}{A vector representing the two outcomes in American odds format.}

\item{precision}{A numerical value representing the precision. The default precision
is set to 4 digits.}
}
\value{
A vector representing the implied probabilites
}
\description{
Returns a vector of implied probabilities
}
\examples{
calculateImpliedProbPair(c(200, -220))

calculateImpliedProbPair(c(1000, -800))

calculateImpliedProbPair(c(1000, -800), precision = 7)
}
