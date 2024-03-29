% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trinomial.R
\name{tri.calc.sample}
\alias{tri.calc.sample}
\title{Calculate needed sample size for election auditing using the
Trinomial Bound}
\usage{
tri.calc.sample(
  Z,
  beta = 0.75,
  guess.N = 20,
  p_d = 0.1,
  swing = 5,
  power = 0.9,
  bound = c("e.plus", "WPM", "passed")
)
}
\arguments{
\item{Z}{elec.data object}

\item{beta}{1-beta is the acceptable risk of failing to notice that
a full manual count is needed given an election with an actual
outcome different from the semi-official outcome.}

\item{guess.N}{The guessed needed sample size.}

\item{p_d}{For the alternate: estimate of the proportion of
precincts that have error.}

\item{swing}{For the alternate: estimate of the max size of an
error in votes, given that error exists.}

\item{power}{The desired power of the test against the specified
alternate defined by p\_d and swing.}

\item{bound}{e.plus, WPM, or use the passed, previously computed,
e.max values in the Z object.}
}
\value{
An \code{audit.plan.tri} object.  This is an object that
  holds information on how many samples are needed in the audit,
  the maximum amount of potential overstatement in the election,
  and a few other things.
}
\description{
Calculate an estimated sample size to do a trinomial bound that
would have a specified power (the chance to certify assuming a
given estimate of low-error error rate), and a specified maximum
risk of erroneously certifying if the actual election outcome is
wrong.
}
\examples{

data(santa.cruz)
Z = elec.data( santa.cruz, C.names=c("danner","leopold") )
tri.calc.sample( Z, beta=0.75, guess.N = 10, p_d = 0.05,
               swing=10, power=0.9, bound="e.plus" )

}
\references{
See Luke W. Miratrix and Philip B. Stark.  (2009)
  Election Audits using a Trinomial Bound.
  http://www.stat.berkeley.edu/~stark
}
\seealso{
See \code{\link{elec.data}} for information on the object that
holds vote counts.  See \code{\link{tri.sample}} for drawing the
actual sample.  The \code{audit.plan.tri} object holds the audit
plan information (e.g., number of draws, estimated work in ballots
to audit, etc.).  See \code{\link{trinomial.bound}} for analyzing
the data once the audit results are in.  See
\code{\link{tri.audit.sim}} for simulating audits using this
method.  See \link{CAST.audit} for an SRS audit method.
}
