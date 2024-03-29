% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elec_functions.R
\name{find.stratification}
\alias{find.stratification}
\title{find.stratification}
\usage{
find.stratification(D, aud, strat.col)
}
\arguments{
\item{D}{Table of votes}

\item{aud}{Table of audit data}

\item{strat.col}{The column to use that identifies the stratification levels}
}
\value{
Table of strata. For each stratum (row) the table has the name of
the stratam, the number of precincts in the stratum, the number of audited
precincts and percent of precincts audited.
}
\description{
Find how audit covered the strata for a given table of votes and audits.
}
\author{
Luke W. Miratrix
}
