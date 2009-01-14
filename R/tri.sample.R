`tri.sample` <-
function( Z, n, seed=NULL, 
						print.trail=FALSE,
						simplify=TRUE,
						return.precincts = TRUE, 
						PID = "PID",
						known="known" ) {
	
	if ( is.audit.plan.tri( n ) ) {
		Z$V$e.max = switch( n$bound,
		 	"passed" = {
				if ( is.null( Z$V$e.max ) ) {
					stop( "No e.max column, and told to not calculate it" )
				}
				Z$V$e.max },
			"e.plus" = maximumMarginBound( Z ),
			"WPM" = 0.4 * Z$V[[Z$tot.votes.col]] / Z$margin
			)
		n = n$n 
	} else {
		if ( is.null(Z$V$e.max) ) {
			warning( "Computing e.max values to sample from" )
			Z$V$e.max = maximumMarginBound( Z )
		}
	}	
		
	stopifnot( n > 0 && n <= Z$N )
	
		
	if ( !is.null(seed) ) {
		cat( "setting seed to ", seed )
		set.seed( seed )	
	}
	
	stopifnot( !is.null( Z$V$e.max ) )
	
	## Note the replace=TRUE--technically, we should sample a SRS of vote units
	## but since we are looking at margins, "vote units" are abstract, arb small 
	## pieces of the margin.  So just sample with replacement.
	sample = sample( 1:nrow(Z$V), n, prob=Z$V$e.max, replace=TRUE )
	
	A = Z$V[sample,]
	A = A[ order(A[[PID]]), ]
	if ( simplify ) {
		A$count = table(A$PID)[ A$PID ]
		A = A[ !duplicated(A$PID), ]
	}
	if ( return.precincts ) {
		A
	} else {
		tri.sample.stats( A )
	}
}

