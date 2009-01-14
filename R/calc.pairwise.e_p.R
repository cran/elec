`calc.pairwise.e_p` <-
function( Z, audit=NULL, err.override=NULL ) {
  ## Calculate the error by finding the maximum margin reduction for each precint
  ## by computing all margin reductions between pairs of winners & losers
  ## (scaling by that pair's total margin to get a proportion) and then
  ## taking the max of all such proportions (usually will be the last 
  ## winner to the closest loser).
  ## Param err.override:  If non-null, use this as the found error in votes rather than
  ##                      the actual errors found in the audit.
  ## Return: Vector (of length of audited precincts) of found errors by precinct. 

 
  if ( is.null( audit ) ) {
 	audit = Z$audit
  }
  if ( is.null(audit) ) {
    stop( "No audit to calculate e_p values for." )
  }

  stopifnot( !is.null(audit$PID ) )
  rownames(audit) = audit$PID
  stopifnot( Z$C.names %in% names(audit) )
  
  err.bound = data.frame( row.names = row.names(audit) )
  
  for ( i in Z$winners ) {
    for ( j in Z$losers ) {
      
      mrg = paste(i,j,sep="_")

      if ( is.null(err.override) ) {
        err.bound[[mrg]] =  (audit[[i]]-audit[[j]]) / Z$Ms[[i,j]]
      } else {
        if ( is.null( audit[[Z$tot.votes.col]] ) ) {
          warning( "No '", Z$tot.votes.col, "' in audit matrix in calc.pairwise.e_p" )
          err.bound[[mrg]] = err.override/Z$Ms[[i,j]]
        } else {
          err.bound[[mrg]] = pmin( audit[[Z$tot.votes.col]], err.override)/Z$Ms[[i,j]]
        }
      }
    }
  }

  apply( err.bound, 1, max )
}

