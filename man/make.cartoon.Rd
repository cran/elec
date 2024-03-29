% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_truth_functions.R
\name{make.cartoon}
\alias{make.cartoon}
\title{Make the cartoon example from the CAST paper as a voter data
matrix.}
\usage{
make.cartoon(n = 400, vote.dist = c(125, 113, 13), stratify = TRUE)
}
\arguments{
\item{n}{Size of sample.}

\item{vote.dist}{reported votes for C1, C2, and C3 in order for all
precincts.prompt}

\item{stratify}{Should the sample be stratified?}
}
\description{
This makes the sample scenario described in P. B. Stark's CAST
paper.
}
