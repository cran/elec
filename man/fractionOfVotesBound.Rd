% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elec_functions.R
\name{fractionOfVotesBound}
\alias{fractionOfVotesBound}
\title{Fraction of votes bound}
\usage{
fractionOfVotesBound(Z, frac = 0.4)
}
\arguments{
\item{Z}{The elec.data object.}

\item{frac}{Fraction of total votes that could be a winner
overstatement/loser understatement.  So if the worst-case is a
20\% flip then enter 0.4}
}
\description{
WPM.  The maximum error of the unit is a fixed
percentage of the total votes cast in the unit.  Typically the 20\% WPM is
used--meaning a swing of 40\% is the largest error possible as 20\% of the
votes go from the winner to the loser.
}
\seealso{
maximumMarginBound
}
