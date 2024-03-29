% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elec_functions.R
\name{compute.audit.errors}
\alias{compute.audit.errors}
\title{Calculate the measured error in each of the audited precicnts.}
\usage{
compute.audit.errors(
  Z,
  audit = NULL,
  calc.e_p = calc.pairwise.e_p,
  w_p = weight.function("no.weight"),
  bound.col = "tot.votes",
  err.override = NULL
)
}
\arguments{
\item{Z}{Elec.data object holding the originally reported results}

\item{audit}{A data.frame with one column per candidate that holds
the totals from the audit.  Each row corresponds to a precinct.
Object needs a PID column with precinct ids that match the ones
in Z.}

\item{calc.e_p}{Calculate e\_p or take as given.}

\item{w_p}{The weight function to use to reweight the errors of
precincts.}

\item{bound.col}{This is the vector (in audit) containing the
maximum number of votes possible in the various precincts.}

\item{err.override}{If non-null, use this as the found error in
votes rather than the actual errors found in the audit.}
}
\value{
Orig audit table from Z with two new columns, err and
  err.weighted, corresponding to the errors found in each audited
  precinct before and after the weight function has been applied to
  them.
}
\description{
Calculate the measured error in each of the audited precicnts.
}
