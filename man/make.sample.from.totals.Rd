% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_sample.R
\name{make.sample.from.totals}
\alias{make.sample.from.totals}
\title{Make sample from vote totals (for simulations)}
\usage{
make.sample.from.totals(vote.W, vote.L, totals)
}
\arguments{
\item{vote.W}{Total votes for winner.}

\item{vote.L}{Total votes for loser.}

\item{totals}{Vector of total votes for precincts.}
}
\description{
Given a vector of precinct totals and the total votes for the winner
and the loser, make a plausible precinct-by-precinct vote count that
works. 
Note: the margins of the precincts will all be the same as the margin
of the overall race.
}
