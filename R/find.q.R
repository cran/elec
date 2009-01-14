`find.q` <-
function( V, t.stat, bound.col, M, threshold=1.0,
			 w_p = weight.function("no.weight"),
 			 drop=NULL ) {
  ## Find q, the minimum number of precints with w_p's greater than given t.stat
  ## that can hold an entire election shift in them.

  ## I.e., find the number of precints that need to have "large taint" in order to
  ## flip the election.  This is, essentially, finding a collection of precints
  ## such that the max error (e.max) plus the background error (the w_p-inverse of the
  ## t.stat) for the rest of the precints is greater than the margin (or 1 if done
  ## by proportions).
  ##
  ##
  ## Param bound.col:  The name of the column in V to be used for the passed size 
  ##                   (max # votes, total votes, incl undervotes, etc.) to the error 
  ##                   function.
  ##       threshold:  The total amount of error to pack in the set of tainted precincts
  ##            drop:  Drop precincts with this column having a "true" value--they are
  ##                   previously audited or otherwise known, and thus can't hold error.
  ##                   Can also pass a logical T/F vector of the length of nrow(V)
  ##
  ## Return:  integer, number of badly tainted precints needed to hold 'threshold' error
  
  stopifnot( is.null( V$e.max ) == FALSE )
  
  if ( !is.null( drop ) ) {
    if ( length(drop) == nrow(V) ) {
      V = V[ !drop, ]
    } else {
      V = V[ !V[[drop]], ]
    }
  }
  
  sortErr = data.frame( e.max=V$e.max, wp.inv=w_p[[2]](t.stat, V[[bound.col]], M  ) )
  
  sortErr$bkg = pmin(sortErr$e.max, sortErr$wp.inv )

  sortErr$importance = sortErr$e.max - sortErr$bkg
  sortErr = sortErr[order( sortErr$importance, decreasing=TRUE ),]

  q = 0
  totErr = sum( sortErr$bkg )
  if ( sum( sortErr$importance ) + totErr < threshold ) {
    q = nrow(sortErr)
  } else {
    while( totErr < threshold ) {
      q = q + 1;
      totErr = totErr + sortErr$importance[q]
    }
  }
  q
}

