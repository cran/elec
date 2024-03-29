% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elec-package.R
\docType{data}
\name{yolo}
\alias{yolo}
\title{Yolo County, CA Election Data}
\format{
A data frame with 114 observations on the following 8 variables.
\describe{ \item{PID}{ Unique identifier for the batches of ballots}
\item{Pct}{The precinct id of the batch} \item{how}{Vote by
mail (VBM) or walk-in (PCT)} \item{b}{Number of votes cast in that
unit} \item{under}{Number of undervotes (ballots not voted).}
\item{over}{Number of overvotes (where someone marked both yes and
no).} \item{y}{Reported number of valid ballots marked yes.}
\item{n}{Reported number of valid ballots marked no.} }
}
\source{
Yolo County, CA.  Special thanks to Freddie Oakley and Tom
Stanionis.
}
\description{
This is for measure W in Yolo County, CA, November 2008.  The file includes
precinct-level reports.

In the actual audit, 6 precincts were selected (see example) and audited by
hand-to-eye count by a group of 4 people cross-checking each other. One of
the 6 batches had underreported the "yes" votes by 1, and one had
overreported the "yes" votes by 1.  There were no other errors.
}
\examples{

# Make an elec.data object out of precicnt-level results
data(yolo)
yolo = elec.data( yolo, C.names=c("y","n","under","over"), tot.votes.col="b" ) 

# Look at different sample sizes and cuts for setting aside
# small precincts
CAST.calc.opt.cut( yolo, beta=0.75, stages=1, t=5, plot=TRUE )

print( yolo )

# Get details of the audit plan -- expected work, etc.
ap <- CAST.calc.sample( yolo, beta=0.75, stages=1, t=5, small.cut=5 )
print( ap )

# Draw a sample (seed not used for actual audit)
CAST.sample(yolo, ap, seed=12345678)



}
\references{
See Stark et al. for papers using this data to illustrate
risk-limiting audits of election data.
}
\keyword{datasets}
