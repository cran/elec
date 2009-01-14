`CAST.sample` <-
function( Z,  ns,  strata=NULL, seed=NULL, 
						print.trail=FALSE, known="known" ) {
	pt = function( ... ) { if ( print.trail ) { cat( ..., "\n" ) } }
		
	if ( is.audit.plan(ns) ) {
		strata = ns$strata
		ns = ns$ns
	}
	
	if ( !is.null( seed ) ) {
          set.seed( seed )
          pt( "Setting seed to ", seed )
	}
	
	if ( !is.null( Z$V[[known]] ) ) {
		V = Z$V[ !Z$V$known, ]
	} else {
		V = Z$V
	}

	if ( is.null( strata ) ) {
		stopifnot( length(ns) == 1 )
	
		ids = sample( nrow(V), ns )
		pt( "Full sample: ", ids )
		
		V$PID[ids]

	} else {
		strat = split( V, V[[strata]] )
	
		l = lapply( 1:length(ns), function( X ) {
			st = names(ns)[[X]]
			pids = sort( strat[[ st ]]$PID )
			ids = sample( length(pids), ns[[X]] )
			pt( "Strata", st, ": ", ids )
		
			pids[ids]
		} )
		unlist( l )
	}
}

