% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/betting_tools.R
\name{calculateTheoreticalHold}
\alias{calculateTheoreticalHold}
\title{Calculates the theoretical hold}
\usage{
calculateTheoreticalHold(pair, precision = 4)
}
\arguments{
\item{pair}{A vector representing the pair of outcomes in American odds format}

\item{precision}{A numerical value representing the precision. The default precision
is set to 4 digits.}
}
\value{
A numerical value representing the theoretical hold.
}
\description{
Returns the theoretical hold for a two-outcome line set. This corresponds
to the profit a sportsbook would expect to make were a player to bet on either side
of an event with all else being equal.
}
\examples{
calculateTheoreticalHold(c(-110, -110))

calculateTheoreticalHold(c(-1500, 875))

calculateTheoreticalHold(c(-1500, 875), precision = 7)
}
