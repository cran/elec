`find.stark.SRS.p` <-
function( N, n, q ) {
                                        ## Find the p-value for a given q, n, and N
  
                                        ## N = total number of precints
                                        ## n = total number of audited precints (must be less than N)
                                        ## q = min number of precints that could hold taint to flip election
  
  if ( n >= N ) stop( "Audit size is equal or greater than population size." )
  
                                        #cat( "N=",N, "n=",n,"q=",q,"\n")
  if ( N-q < n ) {
    0
  } else {
                                        # q bad precints, N-q good ones, n draws -> chance of getting
                                        # no bad precints in audit
    phyper( 0, q, N-q, n )
  }
}

