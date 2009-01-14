`make.truth.opt.bad` <-
function(Z, strata="strata", 
			bound=c("margin","WPM"), t=0 ) {
	bound = match.arg( bound )
	
	if ( bound=="WPM" ) {
		s = CAST.calc.sample(Z, t=t, bound.function=fractionOfVotesBound )
	} else {
		s = CAST.calc.sample(Z, t=t )
	}
	
	winner = Z$winner[[Z$f]]
	
	stopifnot( all( s$Z$V$PID == Z$V$PID ) )
	V = Z$V[ order(s$Z$V$e.max, decreasing=TRUE), ]
	baddies = 1:nrow(V) <= s$q
	V[winner] = pmax( 0,  V[[winner]] - t )
	
	tots = V$tot.votes[baddies]
	if ( bound=="margin" ) {
		newvotes = as.data.frame( matrix( 0, nrow=s$q, ncol=length(Z$C.names) ) )
		names(newvotes) = Z$C.names
		newvotes[[ Z$losers[[1]] ]] = tots
		V[ baddies, Z$C.names ] = newvotes 
	} else {
		V[ baddies, Z$losers[[1]] ] = V[ baddies, Z$losers[[1]] ] + round( 0.4 * V[ baddies, ]$tot.votes )
	}

	
	nZ = make.Z( V[ Z$V$PID, ], Z$C.names )
	stopifnot( nZ$winner != Z$winner )
	nZ$num.tweaked = s$q
	nZ
	
}

