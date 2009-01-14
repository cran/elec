`make.truth.opt.bad.strat` <-
function(Z, strata="strata", t=3, shuffle.strata=FALSE) {
	s = CAST.calc.sample(Z, t=t)
	
	if ( shuffle.strata ) {
		pids = sample(Z$V$PID, s$q )
	} else {
		strats = split( Z$V$PID, Z$V[strata] )
		pids = lapply( names(s$stratas), function( ST ) {
			sample( strats[[ST]], floor( s$q * s$stratas[[ST]] / s$N ) )
		} )
		pids = unlist( pids )
		if ( ( ext <- ( s$q - length( pids ) ) ) > 0 ) {
			pids = c( pids, sample( setdiff( Z$V$PID, pids ), ext ) )
		}
	}
		
	Z$V["C1"] = Z$V["C1"] - t
	baddies = Z$V$PID %in% pids
	Z$V[ baddies, Z$C.names ] = cbind( rep( 0, s$q ), rep(255,s$q), rep(0,s$q) ) 
	make.Z( Z$V, Z$C.names )
}

