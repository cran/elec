% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CAST.R
\name{CAST.calc.opt.cut}
\alias{CAST.calc.opt.cut}
\title{Calculate Optimal CAST plan}
\usage{
CAST.calc.opt.cut(Z, beta = 0.9, stages = 2, t = 3, plot = FALSE, ...)
}
\arguments{
\item{Z}{The elec.data object}

\item{beta}{1-\code{beta} is the risk of the audit failing to notice the
need to go to a full manual count if it should.}

\item{stages}{Number of stages in the audit.}

\item{t}{The allowed vote swing that is not considered a material error.}

\item{plot}{TRUE/FALSE.  Plot the trade-off curve.}

\item{\dots}{Extra arguments to the plot command.}
}
\value{
Returns a list.  \item{cut}{ Size of the optimal cut.  All precincts
with an error smaller than or equal to cut would not be audited, and instead
be assumed to be in full error. } \item{n}{ Corresponding needed sample size
given that cut. } \item{q}{ The number of tainted precincts that would be
needed to throw the election, beyond the ones set aside due to being smaller
than \code{cut}.}
}
\description{
With CAST, it is sometimes advantageous to set aside small precincts and
assume they are entirely in error so as to reduce the total number of
precincts in the pool that we sample from.  This trade-off can increase the
power of the audit or, in other terms, allow us to sample fewer precincts as
the chance of nabbing the large, dangerous ones is larger.
}
\details{
Of all cuts that produce the smallest \code{n}, it returns the smallest cut
(since sometimes multiple cut-offs lead to the same sample size).

This function also plots the trade-off of sample size for a specific cut, if
the plot flag is TRUE.

This function iteratively passes increasing values of \code{small.cut} to
\code{\link{CAST.calc.sample}} and examines the resulting \code{n}.
}
\examples{


        ## Find optimial cut for  determining which small precincts that
        ## we would set aside and not audit in Santa Cruz
        data(santa.cruz)
        Z = elec.data( santa.cruz, C.names=c("leopold","danner") )

        CAST.calc.opt.cut( Z, beta=0.75, stages=1, t=5, plot=TRUE )

}
\author{
Luke W. Miratrix
}
