`stark.test` <-
function( votes, audits, C.names=NULL, f=1, pool=TRUE, pairwise=FALSE, ... ) {
  ## Do the entire test. Basically a driver function that sets up 'Z' matrix and passes buck
  ## to the stark.test.Z
  ## 
  ## param       votes:  Table of reported votes.  Each row is precinct.
  ## param      audits:  Table of audits.  Each row is precinct.  Table reports overstatement by
  ##                     candidate.
  ## param     C.names:  Names of candidates (and names of cor columns in votes and audits tables.
  ## param           f:  The number of winners
  ## param    pairwise:  if TRUE then do a pairwise test for all pairs and return
  ##                     highest p-value
  ## param     C.names:  if NULL will derive from cols 2 on of votes
  ## param   strat.col:  if NULL will not stratify

  if ( pairwise ) {
    stark.pairwise.test( votes, audits, C.names=C.names, f=f, pool=pool, ... )
  } else {
    Z = make.Z(votes, C.names, f, audit=audits, pool=pool )

    T = stark.test.Z( Z, ... )
    
    T
  }
}

