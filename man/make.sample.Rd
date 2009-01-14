\name{make.sample}
\alias{make.sample}
\alias{make.sample.from.totals}
\alias{make.sample.from.totals.margin}
\alias{make.cartoon}

\title{ make.sample and friends }
\description{
These methods are for SIMULATION STUDIES.  These functions will build a sample, i.e. simulated, record of votes
given certain parameters.
}
\usage{
make.cartoon(n = 400, vote.dist = c(125, 113, 13),
             stratify = TRUE)
make.sample(M, N, strata = 1, per.winner = NULL,
                 worst.e.max = NULL, R = NULL,
                 tot.votes = 1e+05)
make.sample.from.totals(vote.W, vote.L, totals)
make.sample.from.totals.margin(M, totals, per.winner = NULL)
}

\arguments{
  \item{M}{ The margin desired between the winner and loser. }
  \item{N}{ Number of precincts desired. }
  \item{strata}{ Number of strata desired. }
  \item{per.winner}{ The percent of votes the winner should receive. }
  \item{worst.e.max}{ The worst e.max possible for any precinct. }
  \item{R}{ The "dispersion" a measure of how unequal in size precincts
  should be.  R needs to be greater than 2.  NULL indicates equal size. }
  \item{tot.votes}{ The total votes desired. }
  \item{vote.W}{ Total votes for winner. }
  \item{vote.L}{ Total votes for loser. }
  \item{totals}{ Vector of total votes for precincts. }
  \item{vote.dist}{ reported votes for C1, C2, and C3 in order for all precincts.prompt }
  \item{n}{Size of sample.}
  \item{stratify}{ Should the sample be stratified? }
}
\details{
  make.cartoon() makes the sample scenario described in Stark's CAST paper.
}
\value{
  A elec.data object meeting the desired specifications.
}
\seealso{
  \code{\link{elec.data}}
  \code{\link{make.truth}}
  \code{\link{do.audit}}
}
\examples{
Z = make.sample(0.08, 150, per.winner=0.4)
Z

Z2 = make.sample(0.08, 150, per.winner=0.4, R=2.2)
Z2

## Note how they have different precinct sizes.

summary(Z$V$tot.votes)
summary(Z2$V$tot.votes)


}
\references{ See http://www.stat.berkeley.edu/~stark/Vote/index.htm for relevant information. }
\author{ Luke W. Miratrix }

