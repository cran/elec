`tri.audit.sim` <-
function( Z, n, p_d = 0.1, 
						swing = 5, 
						return.type = c("statistics","taints","precinct"),
						seed=NULL,
						PID = "PID",
						... )    {
	stopifnot( !is.null( Z$V[[PID]] ) )
	stopifnot( p_d > 0 && p_d < 1 )
	
	return.type = match.arg( return.type )	

	# make the truth.
	Z$V$swing.ep = ifelse( runif(Z$N) < p_d, 
			calc.pairwise.e_p( Z, Z$V, err.override=swing ), 0 )
	
	aud = tri.sample( Z, n, PID=PID, simplify=(return.type=="precinct"), ... )

	aud$taint =  aud$swing.ep / aud$e.max
	
	switch( return.type,
		"precinct" = aud,
		"statistics" = c( k=sum( aud$taint > 0 ), d=max( aud$taint ) ),
		"taints" = aud$taint )
}

