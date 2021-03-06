% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/odds_converter.R
\name{Dec2Fair}
\alias{Dec2Fair}
\title{Calculate fair lines from a pair of Decimal odds}
\usage{
Dec2Fair(pair, precision = 4)
}
\arguments{
\item{pair}{A vector representing the two outcomes in Decimal odds format.}

\item{precision}{A numerical value representing the precision. The default precision
is set to 4 digits.}
}
\value{
A vector representing the fair line
}
\description{
Returns a vector of fair lines
}
\examples{
Dec2Fair(c(2.14, 1.86))

Dec2Fair(c(4.00, 1.51))
}
