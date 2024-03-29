% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elec_data.R
\name{elec.data}
\alias{elec.data}
\alias{print.elec.data}
\title{core election audit data structure}
\usage{
elec.data(
  V,
  C.names = names(V)[2:length(V)],
  f = 1,
  audit = NULL,
  pool = TRUE,
  tot.votes.col = "tot.votes",
  PID.col = "PID"
)

\method{print}{elec.data}(x, n = 4, ...)
}
\arguments{
\item{V}{Voter matrix OR 2-element list with Voter Matrix followed by
Candidate names}

\item{C.names}{List of candidate names.  Also names of columns in V}

\item{f}{Number of winners}

\item{audit}{The audit data---must have columns that match C.names.  Columns
are overstatements of votes found for those candidates.}

\item{pool}{Combine small candidates into single pseudo-candidates to
increase power}

\item{tot.votes.col}{Name of column that has the total votes for the
precincts.}

\item{PID.col}{Name of column that identifies unique PIDs for precincts.}

\item{x}{For print() and is.elec.data(). An elec.data object}

\item{n}{Number to print}

\item{...}{The collection of arguments that are passed directly to
elec.data, or (in the case of print), unused.}
}
\value{
A ``elec.data'' data structure.  Note: Will add PID (precinct ID) column if
no PID provided (and generate unique PIDs).  It will rename the PID column
to PID.  Also, rownames are always PIDs (so indexing by PID works).

print: No return value; prints results.
}
\description{
Makes an object (often called a `Z' object in this documentation) that holds
all the vote totals, etc., as well as some precomputed information such as
vote margins between candidates, the theoretical winners, and so on.
}
\details{
\code{elec.data} does some cleaning and renaming of the passed data
structure.  In particular it will rename the tot.votes column to "tot.votes"
if it is not that name already.
}
\examples{

data(santa.cruz)
elec.data( santa.cruz, C.names=c("danner","leopold") )

}
\seealso{
See \link{CAST.audit} for the CAST method.  See
\code{\link{tri.calc.sample}} for the trinomial bound method.  See
\code{\link{countVotes}} for counting the votes listed in Z.
}
\author{
Luke W. Miratrix
}
