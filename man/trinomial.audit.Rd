% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trinomial.R
\name{trinomial.audit}
\alias{trinomial.audit}
\title{Conduct trinomial audit}
\usage{
trinomial.audit(Z, audit)
}
\arguments{
\item{Z}{An elec.data object that is the race being audited.}

\item{audit}{A data.frame with a column for each candidate and a row for
each audited precinct, holding the audit totals for each candidate. An
additional column, \code{count}, holds the number of times that precinct was
sampled (since sampling was done by replacement).}
}
\description{
\code{trinomial.audit} converts the audited total counts for candidates to
overstatements and taints. \code{trinomial.bound} calculates the trinomial
bound given the size of an audit sample, the number of non-zero errors, and
the size of the small-error threshold. It can also plot a contour of the
distribution space, bounds, and alpha lines.
}
\details{
Right now the p-value is computed in a clumsy, bad way.  A grid of points
over (0, xlim) X (0, ylim) is generated corresponding to values of p0 and
pd, and for each point the mean of that distribution and the chance of
generating an outcome as extreme as k is calculated.  Then the set of points
with an outcome close to alpha is extrated, and the corresponding bound is
optimized over this subset.  Not the best way to do things.
}
