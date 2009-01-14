`compute.stark.t` <-
function( Z,
  bound.col,
  calc.e_p=calc.pairwise.e_p,
  w_p = weight.function("no.weight"),
  err.override = NULL,
  return.revised.audit = FALSE
  ) {
  ## Compute the error statistic given audit data and relevant functions
  ##
  ## Param Z              If it already has an audit table with err and err.weighted
  ##                      then it will use those errors, otherwise it will compute them
  ##                      with compute.stark.err
  ##
  ## Param bound.col:     This is the vector containing the maximum number of votes
  ##                      possible in the various precincts.
  ## Param err.override:  If non-null, use this as the found error in votes rather than
  ##                      the actual errors found in the audit.

  ## Return: the test statistic, i.e. the maximum found error in the audit 
  ##         sample, as computed by calc.e_p and weighted by w_p.
  
  if ( is.null( Z$audit[[bound.col]] ) ) {
    warning( "Assuming audit's rownames are unique and correspond to Z$V's row names" )
    Z$audit[[bound.col]] = Z$V[rownames(Z$audit), bound.col]
  }
  if ( is.null( Z$audit$err ) ) {
    Z$audit = compute.audit.errors( Z, bound.col=bound.col, 
                   calc.e_p=calc.e_p, w_p=w_p, err.override=err.override )
  }
  
  t.stat = max( Z$audit$err.weighted )
  
  if ( return.revised.audit ) {
    list( t.stat, Z$audit )
  } else {
    t.stat
  }
}

